#ifndef TEXTURE_H
#define TEXTURE_H

#include <GL/glew.h>
#include <GL/gl.h>
#include <string>

class Texture {
private:
  GLuint tex;

public:
  Texture(GLuint t);

  void Bind();

  static Texture* LoadFromFile(const std::string& filename);
};

#endif