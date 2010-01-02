#include "mainstate.h"

#include <SDL.h>
#include <GL/glew.h>
#include <GL/gl.h>
#include <GL/glu.h>
#include <iostream>

#include "engine.h"
#include "levelfile.h"
#include "maze.h"
#include "player.h"

MainState::MainState(Engine* e) : GameState(e) { }
MainState::~MainState() { }

void MainState::Init() {
  level = LevelFile::Load("data/levels/test1.lvl");

  maze = new Maze(level);
  maze->InitGeometry();

  player = new Player(this);

  lastSecond = 0;
  smallestFPS = 0;
}

void MainState::Resume() {
  glClearColor(1,1,1,1);
  glClearDepth(1);

  glDisable(GL_CULL_FACE);
  glEnable(GL_DEPTH_TEST);

  // Set projection matrix.
  glMatrixMode(GL_PROJECTION);
  glLoadIdentity();
  float screenW = static_cast<float>(engine->GetScreenWidth());
  float screenH = static_cast<float>(engine->GetScreenHeight());
  float aspect = screenW / screenH;
  gluPerspective(90.0f, aspect, 0.1f, 100);
  
  glMatrixMode(GL_MODELVIEW);
}

void MainState::Render() {
  glClear(GL_COLOR_BUFFER_BIT | GL_DEPTH_BUFFER_BIT);

  glLoadIdentity();
  player->SetGLCamera();
  glPushMatrix();

  maze->Render();

  glPopMatrix();
  SDL_GL_SwapBuffers();
}

void MainState::Update() {
  float fps = engine->GetFrameRate();
  if (engine->GetTicks() - lastSecond > 1000) {
    std::cerr << "Framerate :" << fps << std::endl;
    std::cerr << "Smallest fps:" << smallestFPS << std::endl;
    lastSecond = engine->GetTicks();
    smallestFPS = fps;
  }

  if (fps < smallestFPS) { smallestFPS = fps; }

  if (engine->IsKeyPressed(SDLK_LEFT)) {
    player->TurnLeft(engine->GetTimeDelta());
  }
}

LevelFile* MainState::GetLevelFile() {
  return level;
}
