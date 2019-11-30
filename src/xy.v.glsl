attribute vec2 coord2d;
uniform float offset_y;
uniform float scale;

void main(void) {
  gl_Position = vec4((scale * coord2d.x), ( scale * coord2d.y) - offset_y, 0, 1);
}
