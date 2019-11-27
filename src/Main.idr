module Main
import Control.ST
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
import Data.Vect
import Data.Buffer
import CFFI.Memory
import CFFI.Types

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
vsSource: String
vsSource = "#version 120
attribute vec2 coord2d;
void main(void) {
  gl_Position = vec4(coord2d, 0.0, 1.0);
}"

fsSource: String
fsSource = "#version 120
void main(void) {
  gl_FragColor[0] = 0.0;
  gl_FragColor[1] = 0.0;
  gl_FragColor[2] = 1.0;
}";

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
  --   }
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
record MainResources where
  constructor MkResources
  ||| location of the shader program
  program: Int
  shaders: Vect (S (S n)) Int
  ||| locations of all shaders for this program. minimum of two shaders is required (vertex and fragment shader)
  triangleVbo: GLuint
  triangleBuf: Ptr



programFromShaders : IO (MainResources)
programFromShaders = (do
  MkShader program shaderLocs <- createShaders shaderSpec
  traverse printShaderLog shaderLocs
  (vbo :: _ ) <- glGenBuffers 1
  glBindBuffer GL_ARRAY_BUFFER vbo
  ds <- sizeofDouble
  buf <- Glb.doublesToBuffer triangleVertices
  glBufferData GL_ARRAY_BUFFER (ds * (cast $ length triangleVertices)) buf GL_STATIC_DRAW
  pure $ MkResources program shaderLocs vbo buf)
  where shaderSpec: Vect 2 (GLenum, String)
        shaderSpec = [(GL_VERTEX_SHADER, vsSource), (GL_FRAGMENT_SHADER, fsSource)]


mainRender : SDL2.Window -> GLenum -> GLint -> GLuint -> IO ()
mainRender window program attributeCoords vbo = do
  glClearColor 1.0 1.0 1.0 1.0
  glClear GL_COLOR_BUFFER_BIT
  glUseProgram program
  glBindBuffer GL_ARRAY_BUFFER vbo
  glEnableVertexAttribArray attributeCoords
  glVertexAttribPointer attributeCoords 2 GL_DOUBLE GL_FALSE 0 prim__null
  glDrawArrays GL_TRIANGLES 0 3
  glDisableVertexAttribArray attributeCoords
  swapWindow window
  pure ()

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


main : IO ()
main = (do
  window <- sdlGlInitContext width height
  err <- glewInit
  putStrLn $ show err
  initErrorGl
  MkResources program shaderLocs vbo buf <- programFromShaders
  attributeCoords <- glGetAttribLocation program "coord2d"
  loop window program attributeCoords vbo buf
  quit)
    where
      loop : SDL2.Window -> GLenum -> GLuint -> GLuint -> Ptr -> IO ()
      loop window program attributeCoords vbo buf = do
        False <- SDL2.pollEventsForQuit | pure ()
        mainRender window program attributeCoords vbo
        loop window program attributeCoords vbo buf
