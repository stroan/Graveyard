#ifndef VERTEX_H
#define VERTEX_H

#include <vector>

class VertPosCol {
private:
  float data[6];

public:
  VertPosCol(float* pos, float* col);
  VertPosCol(float x, float y, float z, float r, float g, float b);

  static void FillBuffer(const std::vector<VertPosCol>& verts);
  static void SetGLModes();
};

class VertPosTex {
private:
  float data[5];

public:
  VertPosTex(float* pos, float* tex);
  VertPosTex(float x, float y, float z, float u, float v);

  static void FillBuffer(const std::vector<VertPosTex>& verts);
  static void SetGLModes();
};

class VertPosTexNorm {
private:
  float data[8];

public:
  VertPosTexNorm(float* pos, float* tex, float* norm);
  VertPosTexNorm(float x, float y, float z, float u, float v, float nx, float ny, float nz);

  static void FillBuffer(const std::vector<VertPosTexNorm>& verts);
  static void SetGLModes();
};

#endif
