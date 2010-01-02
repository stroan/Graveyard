#include "vertex.h"

#include <boost/foreach.hpp>
#include <GL/glew.h>
#include <GL/gl.h>
#include <memory.h>

VertPosCol::VertPosCol(float* pos, float* col) {
  data[0] = pos[0];
  data[1] = pos[1];
  data[2] = pos[2];
  data[3] = col[0];
  data[4] = col[1];
  data[5] = col[2];
}

VertPosCol::VertPosCol(float x, float y, float z, float r, float g, float b) {
  data[0] = x;
  data[1] = y;
  data[2] = z;
  data[3] = r;
  data[4] = g;
  data[5] = b;
}

void VertPosCol::FillBuffer(const std::vector<VertPosCol>& verts) {
  float* buff = new float[verts.size() * 6];
  float* current = buff;

  BOOST_FOREACH(VertPosCol vert, verts) {
    memcpy(current,vert.data,sizeof(float) * 6);
    current += 6;
  }

  glBufferData(GL_ARRAY_BUFFER, sizeof(float) * 6 * verts.size(), buff, GL_STATIC_DRAW);
}

void VertPosCol::SetGLModes() {
  glEnableClientState(GL_VERTEX_ARRAY);
  glVertexPointer(3, GL_FLOAT, sizeof(float) * 6, 0);
  
  glEnableClientState(GL_COLOR_ARRAY);
  glColorPointer(3, GL_FLOAT, sizeof(float) * 6, (GLuint*)(sizeof(float) * 3));
}
