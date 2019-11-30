module Main
import Graphics.SDL2 as SDL2
import Graphics.Rendering.Gl.Types
import Graphics.Rendering.Gl.Buffers as Glb
import Graphics.Rendering.Gl.Gl41
import Graphics.Rendering.Gl as GL
import Graphics.Util.Transforms
import Graphics.Util.ObjLoader
import Graphics.Util.Mesh
import Graphics.Rendering.Gl
import Graphics.Rendering.Config
import System as System
import Data.Buffer
import Data.SortedMap
import CFFI.Memory
import CFFI.Types
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

squareSize : Int
squareSize = 50
--

-- vsSource : List String
-- vsSource = [
--   "#version 120\n",
--   "attribute vec2 coord2d;                  ",
--   "void main(void) {                        ",
--   "  gl_Position = vec4(coord2d, 0.0, 1.0); ",
--   "}"]
-- fsSource : List String
-- fsSource = [
--   "#version 120\n",
--   "void main(void) {        ",
--   "  gl_FragColor[0] = 0.0; ",
--   "  gl_FragColor[1] = 0.0; ",
--   "  gl_FragColor[2] = 1.0; ",
--   "}"]

glGetShaderiv: Int -> GLenum -> Ptr -> IO ()
glGetShaderiv shader pname params = foreign FFI_C "glGetShaderiv" (Int -> GLenum -> Ptr -> IO ()) shader pname params

glGetProgramiv: Int -> GLenum -> Ptr -> IO ()
glGetProgramiv program pname params = foreign FFI_C "glGetProgramiv" (Int -> GLenum -> Ptr -> IO ()) program pname params

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

foldFun : List Int -> String -> List Int
foldFun acc el = acc ++ [cast $ length el]



myCreateShaders : (shaderSpec: Vect (S (S n)) (GLenum, String)) -> IO Shader
myCreateShaders shaderSpec = do
  locs <- traverse createShader shaderSpec
  programLoc <- glCreateProgram
  traverse (glAttachShader programLoc) locs
  glLinkProgram programLoc
  pure $ MkShader programLoc locs


-- initResources : IO ()
-- initResources = do
--   vs <- glCreateShader GL_VERTEX_SHADER
--   glShaderSource vs 1 vsSource $ foldl foldFun [] vsSource
--   pure ()


  -- char init_resources() {
  --   GLint compile_ok = GL_FALSE, link_ok = GL_FALSE;
  --
  -- 	GLuint vs = glCreateShader(GL_VERTEX_SHADER);
  -- 	const char *vs_source =
  -- 		//"#version 100\n"  // OpenGL ES 2.0
  -- 		"#version 120\n"  // OpenGL 2.1
  -- 		"attribute vec2 coord2d;                  "
  -- 		"void main(void) {                        "
  -- 		"  gl_Position = vec4(coord2d, 0.0, 1.0); "
  -- 		"}";
  -- 	glShaderSource(vs, 1, &vs_source, NULL);
  -- 	glCompileShader(vs);
  -- 	glGetShaderiv(vs, GL_COMPILE_STATUS, &compile_ok);
  -- 	if (!compile_ok) {
  -- 		cerr << "Error in vertex shader" << endl;
  -- 		return 0;
  -- 	}
  --   GLuint fs = glCreateShader(GL_FRAGMENT_SHADER);
  -- 	const char *fs_source =
  -- 		//"#version 100\n"  // OpenGL ES 2.0
  -- 		"#version 120\n"  // OpenGL 2.1
  -- 		"void main(void) {        "
  -- 		"  gl_FragColor[0] = 0.0; "
  -- 		"  gl_FragColor[1] = 0.0; "
  -- 		"  gl_FragColor[2] = 1.0; "
  -- 		"}";
  -- 	glShaderSource(fs, 1, &fs_source, NULL);
  -- 	glCompileShader(fs);
  -- 	glGetShaderiv(fs, GL_COMPILE_STATUS, &compile_ok);
  -- 	if (!compile_ok) {
  -- 		cerr << "Error in fragment shader" << endl;
  -- 		return 0;
  -- 	}
  --   program = glCreateProgram();
  --   glAttachShader(program, vs);
  --   glAttachShader(program, fs);
  --   glLinkProgram(program);
  --   glGetProgramiv(program, GL_LINK_STATUS, &link_ok);
  --   if (!link_ok) {
  --     cerr << "Error in glLinkProgram" << endl;
  --     return 0;
  --   }*-++
  --   const char* attribute_name = "coord2d";
  --   attribute_coord2d = glGetAttribLocation(program, attribute_name);
  --   if (attribute_coord2d == -1) {
  --     cerr << "Could not bind attribute " << attribute_name << endl;
  --     return 0;
  --   }
  --
  --   return 1;
  -- }
--
-- peekReturn : (t: CType) -> (Ptr -> IO ()) -> IO (Int)
-- peekReturn t func = do
--   ptr <- malloc(32)
--   func ptr
--   val <- peek t $ toCPtr ptr
--   mfree ptr
--   pure $ translate t val
--
-- record MainState where
--   constructor MkState
--   offsetX: Double
--   offsetY: Double
--
--

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




record RenderState where
  constructor MkRenderState
  program: GLuint
  window: Ptr
  coord2d: GLint
  offset_x: GLint
  scale_x: GLint

 -- some lookup type destination, resorts to

data Status = Running | Quit

myUniform1d : GLint -> GLdouble -> IO ()
myUniform1d location x =
  foreign FFI_C "myUniform1d" ( GLint -> GLdouble -> IO ()) location x


interface MainState (m : Type -> Type) where
  initWindow: Int -> Int -> ST m Var [add (State Window)]
  initProgram : ST m Var [add (State GLuint)]
  useProg : (prog: Var) -> ST m () [prog ::: (State GLuint)]
  useUniform : (uni: Var) -> GLdouble -> ST m () [uni ::: (State GLint)]
  bindVertexBuffer: (vbo: Var) -> (attrib: Var) -> ST m () [vbo ::: (State GLuint), attrib ::: (State GLint)]
  initShaders : (n: Nat) -> Vect n String -> ST m (Either () Var) [addIfRight (State (Vect n Int))]
  bindShaders : (n: Nat) -> (prog : Var) -> (shaders: Var) -> ST m () [prog ::: (State GLuint), remove shaders (State (Vect n Int))]
  initVarLocs : (prog : Var) -> ST m Var [prog ::: (State GLuint), add (Composite [State GLint, State GLint, State GLint])]
  initBuffer : ST m Var [add (State GLuint)]
  swapWin : (win: Var) -> ST m () [ win ::: (State Window)]
  pollEventsQu : (status: Var) -> ST m () [ win ::: (State Window), status ::: (State Status)]
  pollEvents : (offset: Var) -> (scale: Var) -> ST m () [offset ::: (State Double), scale ::: (State Double)]
  close : (prog: Var) -> (comp: Var) -> ST m () [remove prog (State GLuint), remove comp (Composite [State GLuint, State GLint, State GLint, State GLint])]
  closeb : (n: Nat) -> (prog: Var) -> (comp: Var) -> (shaders: Var) -> ST m () [remove prog (State GLuint), remove comp (Composite [State GLuint, State GLint, State GLint, State GLint]), remove shaders (State (Vect n Int))]


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
  initBuffer = (do
    lift $ putStrLn "init buffer"
    (vbo :: _ ) <- lift $ glGenBuffers 1
    lift $ glBindBuffer GL_ARRAY_BUFFER vbo
    ds <- lift $ sizeofDouble
    -- funBuf <- lift $ pure $ buffer2DFunction (\x => sin (10*x)) 2000 (-1.0) 1.0
    -- weed leaf
    funBuf <- lift $ pure $ polar2DFunction (\theta => (1 + 0.9 * ( cos (8 * theta) ) ) * (1 + 0.1 * (cos (24 * theta ))   ) * (0.9 + 0.1 * (cos (200 *theta))   ) *  (1 + (sin theta)   )) 2000

    buf <- lift $ (Glb.doublesToBuffer funBuf)
    lift $ glBufferData GL_ARRAY_BUFFER (ds * (cast $ length funBuf)) buf GL_STATIC_DRAW
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
    -- lift $ glBindBuffer !(read vbo)

    -- pure ()
  close prog comp = (do
    delete prog
    [a,b,c,d] <- split comp
    delete a; delete b; delete c; delete d; delete comp )
  closeb n prog comp shaders = (do
    [a,b,c,d] <- split comp
    delete prog; delete a; delete b; delete c; delete d; delete comp; delete shaders)

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
  bufferSt <- call initBuffer
  out <- new ()
  stat <- new Running
  offsetSt <- new 0.0
  scaleSt <- new 1.0
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
        loop: (ConsoleIO m, MainState m) => (st: Var) -> ST m () [st ::: (Composite [State Window, State Status, State Double, State Double, State GLuint, State GLuint, Composite [State GLint, State GLint, State GLint]])]
        loop st = (do
          [win, stat, offsetSt, scaleSt, prog, vbo, varlocs] <- split st
          call $ useProg prog
          -- p <- read prog
          --   lift glUseProgram prog
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




--   -- delete shadersSt
--   -- bindShaders ![read shaders]
--   -- closeb 2 prog comp shaders)




    -- shaders <- initShaders ["src/xy.v.glsl","src/xy.f.glsl"]
    -- bindShaders prog shaders


  -- render comp = (do
  --
  --   )
-- mainRender : SDL2.Window -> GLenum -> GLint -> GLuint -> IO ()
-- mainRender window program attributeCoords vbo = do
--   -- glUniform1f(uniform_offset_x, offset_x);
--   -- glUniform1f uniform_scale_x, scale_x);
--   glClearColor 1.0 1.0 1.0 1.0
--   glClear GL_COLOR_BUFFER_BIT
--   glEnable    GL_BLEND
--   glBlendFunc GL_SRC_ALPHA GL_ONE_MINUS_SRC_ALPHA
--   glUseProgram program
--   glBindBuffer GL_ARRAY_BUFFER vbo
--   glEnableVertexAttribArray attributeCoords
--   glVertexAttribPointer attributeCoords 2 GL_DOUBLE GL_FALSE 0 prim__null
--   glDrawArrays GL_TRIANGLES 0 3
--   glDisableVertexAttribArray attributeCoords
--   swapWindow window
--   pure ()
--

-- initMain : (win : Var) -> (varLocs : Var) -> (prog : Var) -> ST m () [win ::: SDL2.Window, varLocs ::: (State VarLocs), prog :: (State GLuint)]
-- initMain win = do


mainRender : SDL2.Window -> GLenum -> GLint -> GLuint -> IO ()
mainRender window program attributeCoords vbo = do
  glClearColor 1.0 1.0 1.0 1.0
  glClear GL_COLOR_BUFFER_BIT
  glEnable    GL_BLEND
  glBlendFunc GL_SRC_ALPHA GL_ONE_MINUS_SRC_ALPHA
  glUseProgram program
  glBindBuffer GL_ARRAY_BUFFER vbo
  glEnableVertexAttribArray attributeCoords
  glVertexAttribPointer attributeCoords 2 GL_DOUBLE GL_FALSE 0 prim__null
  glDrawArrays GL_TRIANGLES 0 3
  glDisableVertexAttribArray attributeCoords
  swapWindow window
  pure ()
    -- offset <- call $ (lift $ (glGetUniformLocation prog "offset_x"))
    -- scale <- call $ (lift $ (glGetUniformLocation prog "scale_x"))
    -- varLocs <- lift $ (MkVarLocs coord2d offset scale)
    -- locs <- new varLocs
    -- pure locs)
    -- Store x = State String
    -- getProgram
    -- connect = do store <- new "Secret Data"
    --              pure store
    -- disconnect store = delete store
    -- readSecret store = read store
    -- login store = do putStr "Enter password: "
    --                  p <- getStr
    --                  if p == "Mornington Crescent"
    --                     then pure OK
    --                     else pure BadPassword
    -- logout store = pure ()




record MainResources where
  constructor MkResources
  ||| location of the shader program
  program: Int
  shaders: Vect (S (S n)) Int
  ||| locations of all shaders for this program. minimum of two shaders is required (vertex and fragment shader)
  vbo: GLuint
  buf: Ptr



programFromShaders : IO (Either FileError MainResources)
programFromShaders = (do
  Right shaderSpec <- readShaderSpec | Left ferror => pure (Left ferror)
  MkShader program shaderLocs <- createShaders shaderSpec
  traverse printShaderLog shaderLocs
  (vbo :: _ ) <- glGenBuffers 1
  glBindBuffer GL_ARRAY_BUFFER vbo
  ds <- sizeofDouble
  buf <- Glb.doublesToBuffer triangleVertices
  glBufferData GL_ARRAY_BUFFER (ds * (cast $ length triangleVertices)) buf GL_STATIC_DRAW
  pure $ Right (MkResources program shaderLocs vbo buf))
  where readShaderSpec: IO(Either FileError (Vect 2 (GLenum, String)))
        readShaderSpec = do
          Right vertexShader <- readFile "src/xy.v.glsl" | Left ferror => pure (Left ferror)
          Right fragmentShader <-  readFile "src/xy.f.glsl" | Left ferror => pure (Left ferror)
          pure $ Right [(GL_VERTEX_SHADER, vertexShader), (GL_FRAGMENT_SHADER, fragmentShader)]

  --
  -- glClearColor(1.0, 1.0, 1.0, 1.0);
	-- glClear(GL_COLOR_BUFFER_BIT);
  --
	-- glUseProgram(program);
	-- glEnableVertexAttribArray(attribute_coord2d);
	-- GLfloat triangle_vertices[] = {
	--     0.0,  0.8,
	--    -0.8, -0.8,
	--     0.8, -0.8,
	-- };
	-- /* Describe our vertices array to OpenGL (it can't guess its format automatically) */
	-- glVertexAttribPointer(
	-- 	attribute_coord2d, // attribute
	-- 	2,                 // number of elements per vertex, here (x,y)
	-- 	GL_FLOAT,          // the type of each element
	-- 	GL_FALSE,          // take our values as-is
	-- 	0,                 // no extra data between each position
	-- 	triangle_vertices  // pointer to the C array
	-- 					  );
  --
	-- /* Push each element in buffer_vertices to the vertex shader */
	-- glDrawArrays(GL_TRIANGLES, 0, 3);
  --
	-- glDisableVertexAttribArray(attribute_coord2d);
  --
	-- /* Display the result */
	-- SDL_GL_SwapWindow(window);

  -- /* Push each element in buffer_vertices to the vertex shader */
  -- glDrawArrays(GL_TRIANGLES, 0, 3);
  --
  -- glDisableVertexAttribArray(attribute_coord2d);
  --
  -- /* Display the result */
  -- SDL_GL_SwapWindow(window);
  --
        --
        -- /* Clear the background as white */
      	-- glClearColor(1.0, 1.0, 1.0, 1.0);
      	-- glClear(GL_COLOR_BUFFER_BIT);
        --
      	-- glUseProgram(program);
      	-- glEnableVertexAttribArray(attribute_coord2d);
      	-- GLfloat triangle_vertices[] = {
      	--     0.0,  0.8,
      	--    -0.8, -0.8,
      	--     0.8, -0.8,
      	-- };
      	-- /* Describe our vertices array to OpenGL (it can't guess its format automatically) */
      	-- glVertexAttribPointer(
      	-- 	attribute_coord2d, // attribute
      	-- 	2,                 // number of elements per vertex, here (x,y)
      	-- 	GL_FLOAT,          // the type of each element
      	-- 	GL_FALSE,          // take our values as-is
      	-- 	0,                 // no extra data between each position
      	-- 	triangle_vertices  // pointer to the C array
      	-- 					  );
        --
      	-- /* Push each element in buffer_vertices to the vertex shader */
      	-- glDrawArrays(GL_TRIANGLES, 0, 3);
        --
      	-- glDisableVertexAttribArray(attribute_coord2d);
        --
      	-- /* Display the result */
      	-- SDL_GL_SwapWindow(window);
        --
--
--
main : IO ()
main = run startMain
-- main = (do
--   window <- sdlGlInitContext width height
--   err <- glewInit
--   putStrLn $ show err
--   initErrorGl
--   Right (MkResources program shaderLocs vbo buf) <- programFromShaders |
--     Left ferror => (do
--       putStrLn $ show ferror
--       quit)
--   attributeCoords <- glGetAttribLocation program "coord2d"
--   loop window program attributeCoords vbo buf
--   quit)
--     where
--       loop : SDL2.Window -> GLenum -> GLuint -> GLuint -> Ptr -> IO ()
--       loop window program attributeCoords vbo buf = do
--         False <- SDL2.pollEventsForQuit | pure ()
--         mainRender window program attributeCoords vbo
--         loop window program attributeCoords vbo buf
