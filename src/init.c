#include <GL/glew.h>
/* Using SDL2 for the base window and OpenGL context init */
#include <SDL.h>

GLint attribute_coord2d;
GLuint program;
const char *vs_source = //"#version 100\n"  // OpenGL ES 2.0
"#version 120\n"  // OpenGL 2.1
"attribute vec2 coord2d;                  "
"void main(void) {                        "
"  gl_Position = vec4(coord2d, 0.0, 1.0); "
"}";

const char *fs_source =
  //"#version 100\n"  // OpenGL ES 2.0
  "#version 120\n"  // OpenGL 2.1
  "void main(void) {        "
  "  gl_FragColor[0] = 0.0; "
  "  gl_FragColor[1] = 0.0; "
  "  gl_FragColor[2] = 1.0; "
  "}";

void myUniform1d(GLint loc, GLdouble v0d) {
  GLfloat v0f = v0d;
  glUniform1f(loc, v0f);
}


const char *get_vs_source() {
  return vs_source;
}

const char *get_fs_source() {
  return vs_source;
}


// char init_resources() {
//   GLint compile_ok = GL_FALSE, link_ok = GL_FALSE;
//
// 	GLuint vs = glCreateShader(GL_VERTEX_SHADER);
// 	const char *vs_source =
// 		//"#version 100\n"  // OpenGL ES 2.0
// 		"#version 120\n"  // OpenGL 2.1
// 		"attribute vec2 coord2d;                  "
// 		"void main(void) {                        "
// 		"  gl_Position = vec4(coord2d, 0.0, 1.0); "
// 		"}";
// 	glShaderSource(vs, 1, &vs_source, NULL);
// 	glCompileShader(vs);
// 	glGetShaderiv(vs, GL_COMPILE_STATUS, &compile_ok);
// 	if (!compile_ok) {
// 		cerr << "Error in vertex shader" << endl;
// 		return 0;
// 	}
//   GLuint fs = glCreateShader(GL_FRAGMENT_SHADER);
// 	const char *fs_source =
// 		//"#version 100\n"  // OpenGL ES 2.0
// 		"#version 120\n"  // OpenGL 2.1
// 		"void main(void) {        "
// 		"  gl_FragColor[0] = 0.0; "
// 		"  gl_FragColor[1] = 0.0; "
// 		"  gl_FragColor[2] = 1.0; "
// 		"}";
// 	glShaderSource(fs, 1, &fs_source, NULL);
// 	glCompileShader(fs);
// 	glGetShaderiv(fs, GL_COMPILE_STATUS, &compile_ok);
// 	if (!compile_ok) {
// 		cerr << "Error in fragment shader" << endl;
// 		return 0;
// 	}
//   program = glCreateProgram();
//   glAttachShader(program, vs);
//   glAttachShader(program, fs);
//   glLinkProgram(program);
//   glGetProgramiv(program, GL_LINK_STATUS, &link_ok);
//   if (!link_ok) {
//     cerr << "Error in glLinkProgram" << endl;
//     return 0;
//   }
//   const char* attribute_name = "coord2d";
//   attribute_coord2d = glGetAttribLocation(program, attribute_name);
//   if (attribute_coord2d == -1) {
//     cerr << "Could not bind attribute " << attribute_name << endl;
//     return 0;
//   }
//
//   return 1;
// }
void GLAPIENTRY
MessageCallback( GLenum source,
                 GLenum type,
                 GLuint id,
                 GLenum severity,
                 GLsizei length,
                 const GLchar* message,
                 const void* userParam )
{
  fprintf( stderr, "GL CALLBACK: %s type = 0x%x, severity = 0x%x, message = %s\n",
           ( type == GL_DEBUG_TYPE_ERROR ? "** GL ERROR **" : "" ),
            type, severity, message );
}

void initErrorGl() {
  // During init, enable debug output
  glEnable              ( GL_DEBUG_OUTPUT );
  glDebugMessageCallback( MessageCallback, 0 );
}



SDL_Window* idris_sdl2_init_gl(int width, int height) {
  if (SDL_Init(SDL_INIT_VIDEO | SDL_INIT_AUDIO |
               SDL_INIT_EVENTS | SDL_INIT_TIMER) < 0) {
    fprintf(stderr, "Unable to init SDL: %s\n", SDL_GetError());
//return NULL;
    exit(1);
  }

  SDL_Window *window = SDL_CreateWindow(
    "sdl2",
    SDL_WINDOWPOS_CENTERED,
    SDL_WINDOWPOS_CENTERED,
    width, height,
    SDL_WINDOW_RESIZABLE | SDL_WINDOW_OPENGL
  );
  SDL_GL_CreateContext(window);

  if (window == NULL) {
    printf("Unable to create window: %s\n", SDL_GetError());
// return NULL;
    exit(1);
  }

  return window;
}
