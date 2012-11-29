#include "texture.h"

#include <SDL.h>

Texture::Texture(GLuint t) : tex(t) { }

void Texture::Bind() {
  glEnable(GL_TEXTURE_2D);
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

Texture* Texture::ColorTexture(int r, int g, int b) {
  char data[] = {r,g,b};
  SDL_Surface* surf = SDL_CreateRGBSurfaceFrom(data, 1, 1, 24, 3, 0x00FF0000, 0x0000FF00, 0x000000FF, 0);


  GLuint tex;
  glGenTextures(1, &tex);
  glBindTexture(GL_TEXTURE_2D, tex);
  glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MIN_FILTER, GL_LINEAR);
  glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MAG_FILTER, GL_LINEAR);
  glTexImage2D(GL_TEXTURE_2D, 0, 3, surf->w, surf->h, 0, GL_RGB, GL_UNSIGNED_BYTE, surf->pixels);

  return new Texture(tex);
}