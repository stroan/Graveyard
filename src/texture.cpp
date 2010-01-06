#include "texture.h"

#include <SDL.h>

Texture::Texture(GLuint t) : tex(t) { }

void Texture::Bind() {
  glBindTexture(GL_TEXTURE_2D, tex);
}

Texture* Texture::LoadFromFile(const std::string& filename) {
  SDL_Surface* surf = SDL_LoadBMP(filename.c_str());
  if (!surf) { return NULL; }

  GLuint tex;
  glGenTextures(1, &tex);
  glBindTexture(GL_TEXTURE_2D, tex);
  glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MIN_FILTER, GL_LINEAR);
  glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MAG_FILTER, GL_LINEAR);
  glTexImage2D(GL_TEXTURE_2D, 0, 3, surf->w, surf->h, 0, GL_BGR, GL_UNSIGNED_BYTE, surf->pixels);

  return new Texture(tex);
}