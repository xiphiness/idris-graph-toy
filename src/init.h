#include <GL/glew.h>
#include <SDL.h>
#include <idris_rts.h>

SDL_Window* idris_sdl2_init_gl(int width, int height);
const char *vs_source;
const char *getvs_source();
void initErrorGl();

void myUniform1d(GLint loc, GLdouble v0d);
