module Main
import Graphics.SDL2 as SDL2
import Graphics.Rendering.Gl.Types
import Graphics.Rendering.Gl.Buffers as Glb
import Graphics.Rendering.Gl.Gl41
import Graphics.Rendering.Gl as GL
-- import Graphics.Util.Transforms
-- import Graphics.Util.ObjLoader
-- import Graphics.Util.Mesh
import Graphics.Rendering.Gl
import Graphics.Rendering.Config
import System as System
import Control.ST
import Data.Vect


%include C "GL/glew.h"
%include C "SDL.h"
%include C "init.h"
%link C "init.o"


fail : (msg: String) -> IO ()
fail msg = do
  err <- getError
  fPutStr stderr $ msg ++ " failed:" ++ err
  fflush stderr
  System.exit 1

width : Int
width = 640

height : Int
height = 480

defaultOffset : Double
defaultOffset = 0.0

defaultScale : Double
defaultScale = 1.0


squareSize : Int
squareSize = 50

initErrorGl: IO ()
initErrorGl = foreign FFI_C "initErrorGl" (IO ())

sdlGlInitContext: Int -> Int -> IO (Window)
sdlGlInitContext width height = do
  windowPtr <- foreign FFI_C "idris_sdl2_init_gl" (Int -> Int -> IO (Ptr)) width height
  pure $ MkWindow windowPtr

swapWindow: Window -> IO ()
swapWindow (MkWindow window) = foreign FFI_C "SDL_GL_SwapWindow" (Ptr -> IO ()) window


triangleVertices: List Double
triangleVertices =
  [
    0.0,  0.8,
   -0.8, -0.8,
    0.8, -0.8
  ]

readShaderSpecs: Vect n String -> IO(Either FileError (Vect n (GLenum, String)))
readShaderSpecs (x0::x1::Nil) = (do
  Right vertexShader <- readFile x0 | Left ferror => pure (Left ferror)
  Right fragmentShader <- readFile x1 | Left ferror => pure (Left ferror)
  pure $ Right ([(GL_VERTEX_SHADER, vertexShader), (GL_FRAGMENT_SHADER, fragmentShader)])
  )
readShaderSpecs (x0::x1::xs) = (do
  Right vertexShader <- readFile x0 | Left ferror => pure (Left ferror)
  Right fragmentShader <- readFile x1 | Left ferror => pure (Left ferror)
  Right remainder <- readShaderSpecs xs | Left ferror => pure (Left ferror)
  pure $ Right ([(GL_VERTEX_SHADER, vertexShader), (GL_FRAGMENT_SHADER, fragmentShader)] ++ remainder)
  )

polar2DFunction : ((Double -> Double) -> Double -> List Double)
polar2DFunction fun res = loop 0 []
  where scale: Double
        scale = (2 * pi) / res
        loop: Double -> List Double -> List Double
        loop i acc =
          let theta = scale * i
              r = fun theta
              x = (cos theta) * r
              y = (sin theta) * r
          in
            if theta <= (2*pi)
              then
                loop (i+1) (acc ++ [x, y])
            else
              acc

buffer2DFunction: ((Double -> Double) -> Double -> Double -> Double -> List Double)
buffer2DFunction func res min max = loop 0 []
  where scale: Double
        scale = (max - min)/res
        loop: Int -> List Double -> List Double
        loop i acc =
          let iDbl = min + (scale * (cast i))
              out = func iDbl
          in
            if iDbl <= max
              then
              if out <= max && out >= min
                then
                loop (i+1) (acc ++ [iDbl, out])
              else loop (i+1) acc
            else
              acc


data Status = Running | Quit

myUniform1d : GLint -> GLdouble -> IO ()
myUniform1d location x =
  foreign FFI_C "myUniform1d" ( GLint -> GLdouble -> IO ()) location x


interface MainState (m : Type -> Type) where
  initWindow: Int -> Int -> ST m Var [add (State Window)]
  initProgram : ST m Var [add (State GLuint)]
  initVarLocs : (prog : Var) -> ST m Var [prog ::: (State GLuint), add (Composite [State GLint, State GLint, State GLint])]
  initShaders : (n: Nat) -> Vect n String -> ST m (Either () Var) [addIfRight (State (Vect n Int))]
  bindShaders : (n: Nat) -> (prog : Var) -> (shaders: Var) -> ST m () [prog ::: (State GLuint), remove shaders (State (Vect n Int))]
  initBuffer : (List Double) -> ST m Var [add (State GLuint)]
  useProg : (prog: Var) -> ST m () [prog ::: (State GLuint)]
  useUniform : (uni: Var) -> GLdouble -> ST m () [uni ::: (State GLint)]
  bindVertexBuffer: (vbo: Var) -> (attrib: Var) -> ST m () [vbo ::: (State GLuint), attrib ::: (State GLint)]
  swapWin : (win: Var) -> ST m () [ win ::: (State Window)]
  pollEventsQu : (status: Var) -> ST m () [ win ::: (State Window), status ::: (State Status)]
  pollEvents : (offset: Var) -> (scale: Var) -> ST m () [offset ::: (State Double), scale ::: (State Double)]

implementation MainState IO where
  initWindow w h = (do
    window <- lift $ (sdlGlInitContext w h)
    lift $ glewInit
    pure !(new window))
  initProgram = do
    lift $ putStrLn "init prog"
    program <- lift $ glCreateProgram
    lift $ putStrLn "initted prog"
    pure !(new program)
  initShaders n shaders = (do
    lift $ putStrLn "init shaders"
    Right shaderSpec <- lift $ readShaderSpecs shaders | Left ferror => (do
      lift $ putStrLn $ show ferror
      pure (Left ()))
    locations <- lift $ traverse createShader shaderSpec
    lift $ traverse printShaderLog locations
    locs <- new locations
    lift $ putStrLn "initted shaders"
    pure (Right locs))
  bindShaders n program shadersSt = (do
    lift $ putStrLn "bind shaders"
    prog <- read program
    shaders <- read shadersSt
    lift $ traverse (glAttachShader prog) shaders
    delete shadersSt
    lift $ glLinkProgram prog
    pure ())
  initVarLocs program = (do
    lift $ putStrLn "initVarLocs"
    prog <- read program
    coord2d <- lift $ glGetAttribLocation prog "coord2d"
    offset <- lift $ glGetUniformLocation prog "offset_y"
    scale <- lift $ glGetUniformLocation prog "scale"
    out <- new ()
    combine out [!(new coord2d), !(new offset), !(new scale)]
    returning out (toEnd out))
  initBuffer bufList = (do
    lift $ putStrLn "init buffer"
    (vbo :: _ ) <- lift $ glGenBuffers 1
    lift $ glBindBuffer GL_ARRAY_BUFFER vbo
    ds <- lift $ sizeofDouble
    buf <- lift $ (Glb.doublesToBuffer bufList)
    lift $ glBufferData GL_ARRAY_BUFFER (ds * (cast $ length bufList)) buf GL_STATIC_DRAW
    pure !(new vbo))
  useProg prog = (do
    lift $ glClearColor 1.0 1.0 1.0 1.0
    lift $ glClear GL_COLOR_BUFFER_BIT
    lift $ glUseProgram !(read prog)
    pure ()
    )
  useUniform uni x = do
    lift $ myUniform1d !(read uni) x
    pure ()
  swapWin win = do
    lift $ swapWindow !(read win)
    pure ()
  pollEventsQu stat = do
    False <- lift $ SDL2.pollEventsForQuit | (do
      write stat Quit
      pure ())
    pure ()
  pollEvents offset scale = (do
    Just (KeyDown key) <- lift $ SDL2.pollEvent | (do pure ())
    case key of
      KeyDownArrow => (do
        write scale ((-0.05) + !(read scale))
        pure ())
      KeyUpArrow => (do
        write scale (0.05 + !(read scale))
        pure ())
      KeyLeftArrow => (do
        write offset ((-0.05) + !(read offset))
        pure ())
      KeyRightArrow => (do
        write offset (0.05 + !(read offset))
        pure ()))
  bindVertexBuffer vbo attrib = (do
    vbos <- read vbo
    attr <- read attrib
    lift $ glBindBuffer GL_ARRAY_BUFFER vbos
    lift $ glEnableVertexAttribArray attr
    lift $ glVertexAttribPointer attr 2 GL_DOUBLE GL_FALSE 0 prim__null
    lift $ glDrawArrays GL_LINE_STRIP 0 2000
    pure ())

startMain : (ConsoleIO m, MainState m) => ST m () []
startMain = (do
  win <- initWindow width height
  prog <- call initProgram
  Right shaderSt <- call $ initShaders 2 shaderList | Left () => (do delete prog; delete win; pure ())
  call $ bindShaders 2 prog shaderSt
  varlocs <- call $ initVarLocs prog
  [coord2d, offset, scale] <- split varlocs
  putStrLn ("coord2d: " ++ (show !(read coord2d)))
  putStrLn ("offset: " ++ (show !(read offset)))
  putStrLn ("scale: " ++ (show !(read scale)))
  combine varlocs [coord2d, offset, scale]
  -- funBuf <- lift $ pure $ buffer2DFunction (\x => sin (10*x)) 2000 (-1.0) 1.0
  bufferSt <- call $ initBuffer (polar2DFunction
    (\theta => (1 + 0.9 * (cos (8 * theta)))
             * (1 + 0.1 * (cos (24 * theta)))
             * (0.9 + 0.1 * (cos (200 * theta)))
             * (1 + (sin theta))) 2000)
  out <- new ()
  stat <- new Running
  offsetSt <- new defaultOffset
  scaleSt <- new defaultScale
  combine out [win, stat, offsetSt, scaleSt, prog, bufferSt, varlocs]
  loop out
  [win, stat, offsetSt, scaleSt, prog, bufferSt, varlocs] <- split out
  delete win; delete offsetSt; delete scaleSt; delete stat; delete prog; delete bufferSt
  [coord2d, offset, scale] <- split varlocs
  delete coord2d; delete offset; delete scale;
  delete varlocs
  delete out
  pure ()
  )
  where shaderList: Vect 2 String
        shaderList = ["src/xy.v.glsl","src/xy.f.glsl"]
        loop: (ConsoleIO m, MainState m) => (st: Var) -> ST m ()
          [st ::: (Composite [State Window, State Status,
                              State Double, State Double,
                              State GLuint, State GLuint,
                              Composite [State GLint, State GLint, State GLint]])]
        loop st = (do
          [win, stat, offsetSt, scaleSt, prog, vbo, varlocs] <- split st
          call $ useProg prog
          [attrib, offset, scale] <- split varlocs
          call $ pollEvents offsetSt scaleSt
          call $ useUniform offset !(read offsetSt)
          call $ useUniform scale !(read scaleSt)
          call $ bindVertexBuffer vbo attrib
          call $ swapWin win
          call $ pollEventsQu stat
          sts <- read stat
          case sts of
            Running => (do
              combine varlocs [attrib, offset, scale]
              combine st [win, stat, offsetSt, scaleSt, prog, vbo, varlocs]
              loop st)
            Quit => (do
              combine varlocs [attrib, offset, scale]
              combine st [win, stat, offsetSt, scaleSt, prog, vbo, varlocs]
              pure ()))


main : IO ()
main = run startMain
