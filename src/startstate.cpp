#include "startstate.h"

#include "engine.h"
#include "texture.h"
#include "mainstate.h"

StartState::StartState(Engine *e) : GameState(e) { }
StartState::~StartState() { }

void StartState::Init() {
  splash = Texture::LoadFromFile("data/textures/splash.bmp");
}

void StartState::Resume() {
  glClearColor(1,1,1,1);
  glClearDepth(1);

  glDisable(GL_CULL_FACE);
  glDisable(GL_DEPTH_TEST);
  glDisable(GL_LIGHTING);

  // Set projection matrix.
  glMatrixMode(GL_PROJECTION);
  glLoadIdentity();
  float screenW = static_cast<float>(engine->GetScreenWidth());
  float screenH = static_cast<float>(engine->GetScreenHeight());
  gluOrtho2D(0, screenW, screenH, 0);
  
  glMatrixMode(GL_MODELVIEW);
}

void StartState::Render() {
  glClear(GL_COLOR_BUFFER_BIT | GL_DEPTH_BUFFER_BIT);

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

void StartState::OnKeyDown(SDL_KeyboardEvent* e) {
  engine->PopState();
  engine->PushState(new MainState(engine, "test1.lvl"));
}