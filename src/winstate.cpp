#include "winstate.h"

#include "engine.h"
#include "texture.h"
#include "mainstate.h"

WinState::WinState(Engine *e) : GameState(e) { }
WinState::~WinState() { }

void WinState::Init() {
  splash = Texture::LoadFromFile("data/textures/won.bmp");

  startTime = engine->GetTicks();
}

void WinState::Resume() {
  glClearColor(1,1,1,1);
  glClearDepth(1);

  glDisable(GL_CULL_FACE);
  glDisable(GL_DEPTH_TEST);
  glDisable(GL_LIGHTING);

  glDisableClientState(GL_VERTEX_ARRAY);
  glDisableClientState(GL_COLOR_ARRAY);
  glDisableClientState(GL_TEXTURE_COORD_ARRAY);
  glDisableClientState(GL_NORMAL_ARRAY);

  glBindBuffer(GL_ARRAY_BUFFER, 0);
  glBindBuffer(GL_ELEMENT_ARRAY_BUFFER, 0);

  // Set projection matrix.
  glMatrixMode(GL_PROJECTION);
  glLoadIdentity();
  float screenW = static_cast<float>(engine->GetScreenWidth());
  float screenH = static_cast<float>(engine->GetScreenHeight());
  gluOrtho2D(0, screenW, screenH, 0);
  
  glMatrixMode(GL_MODELVIEW);
}

void WinState::Render() {
  glClear(GL_COLOR_BUFFER_BIT | GL_DEPTH_BUFFER_BIT);
  glLoadIdentity();

  int left = engine->GetScreenWidth() / 2 - (384/2);
  int right = left + 384;
  int top = engine->GetScreenHeight() / 2 - (256/2);
  int bottom = top + 256;

  splash->Bind();
  glBegin(GL_QUADS);
    glTexCoord2f(0,0); glVertex3f(left,top,1);
    glTexCoord2d(1,0); glVertex3f(right,top,1);
    glTexCoord2f(1,1); glVertex3f(right,bottom,1);
    glTexCoord2d(0,1); glVertex3f(left,bottom,1);
  glEnd();

}

void WinState::OnKeyDown(SDL_KeyboardEvent* e) {
  if (engine->GetTicks() - startTime > 1000) {
    engine->Quit();
  }
}