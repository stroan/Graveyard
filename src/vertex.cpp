#include "vertex.h"

#include <boost/foreach.hpp>
#include <GL/glew.h>
#include <GL/gl.h>
#include <memory.h>

// ************************************************************
// VertPosCol

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

  glDisableClientState(GL_TEXTURE_COORD_ARRAY);
  glDisableClientState(GL_NORMAL_ARRAY);
}

// ************************************************************
// VertPosTex

VertPosTex::VertPosTex(float* pos, float* tex) {
  data[0] = pos[0];
  data[1] = pos[1];
  data[2] = pos[2];
  data[3] = tex[0];
  data[4] = tex[1];
}

VertPosTex::VertPosTex(float x, float y, float z, float u, float v) {
  data[0] = x;
  data[1] = y;
  data[2] = z;
  data[3] = u;
  data[4] = v;
}

void VertPosTex::FillBuffer(const std::vector<VertPosTex>& verts) {
  float* buff = new float[verts.size() * 5];
  float* current = buff;

  BOOST_FOREACH(VertPosTex vert, verts) {
    memcpy(current,vert.data,sizeof(float) * 5);
    current += 5;
  }

  glBufferData(GL_ARRAY_BUFFER, sizeof(float) * 5 * verts.size(), buff, GL_STATIC_DRAW);
}

void VertPosTex::SetGLModes() {
  glEnableClientState(GL_VERTEX_ARRAY);
  glVertexPointer(3, GL_FLOAT, sizeof(float) * 5, 0);
  
  glEnableClientState(GL_TEXTURE_COORD_ARRAY);
  glTexCoordPointer(2, GL_FLOAT, sizeof(float) * 5, (GLuint*)(sizeof(float) * 3));

  glDisableClientState(GL_COLOR_ARRAY);
  glDisableClientState(GL_NORMAL_ARRAY);
}

// ************************************************************
// VertPosTexNorm


VertPosTexNorm::VertPosTexNorm(float* pos, float* tex, float* norm) {
  data[0] = pos[0];
  data[1] = pos[1];
  data[2] = pos[2];
  data[3] = tex[0];
  data[4] = tex[1];
  data[5] = norm[0];
  data[6] = norm[1];
  data[7] = norm[2];
}

VertPosTexNorm::VertPosTexNorm(float x, float y, float z, float u, float v, float nx, float ny, float nz) {
  data[0] = x;
  data[1] = y;
  data[2] = z;
  data[3] = u;
  data[4] = v;
  data[5] = nx;
  data[6] = ny;
  data[7] = nz;
}

void VertPosTexNorm::FillBuffer(const std::vector<VertPosTexNorm>& verts) {
  float* buff = new float[verts.size() * 8];
  float* current = buff;

  BOOST_FOREACH(VertPosTexNorm vert, verts) {
    memcpy(current,vert.data,sizeof(float) * 8);
    current += 8;
  }

  glBufferData(GL_ARRAY_BUFFER, sizeof(float) * 8 * verts.size(), buff, GL_STATIC_DRAW);
}

void VertPosTexNorm::SetGLModes() {
  glEnableClientState(GL_VERTEX_ARRAY);
  glVertexPointer(3, GL_FLOAT, sizeof(float) * 8, 0);
  
  glEnableClientState(GL_TEXTURE_COORD_ARRAY);
  glTexCoordPointer(2, GL_FLOAT, sizeof(float) * 8, (GLuint*)(sizeof(float) * 3));

  glEnableClientState(GL_NORMAL_ARRAY);
  glNormalPointer(GL_FLOAT,  sizeof(float) * 8, (GLuint*)(sizeof(float) * 5));

  glDisableClientState(GL_COLOR_ARRAY);
}