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

#endif
