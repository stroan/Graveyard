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

#include "Model_3DS.h"

MainState::MainState(Engine* e) : GameState(e) { }
MainState::~MainState() { }

void MainState::Init() {
  level = LevelFile::Load("data/levels/test1.lvl");

  maze = new Maze(level);
  maze->InitGeometry();

  player = maze->GetPlayer();

  sphere = new Model_3DS();
  sphere->Load("data/models/sphere.3DS");
  sphere->shownormals = true;
  sphere->lit = false;

  badguy = new Model_3DS();
  badguy->Load("data/models/badguy.3DS");
  badguy->scale = 0.015f;

  badguyGunLeft = badguy->GetObjectByName("LeftGun");
  badguyGunRight = badguy->GetObjectByName("RightGun");
  gunRotLeft = true;

  lastSecond = 0;
  smallestFPS = 0;
  wireframe = false;
}

void MainState::Resume() {
  glClearColor(0,0.1,0.1,1);
  glClearDepth(1);

  glDisable(GL_CULL_FACE);
  glEnable(GL_DEPTH_TEST);

  glEnable(GL_LIGHTING);
  float ambient[] = {0.1f, 0.1f, 0.1f, 1.0f};
  float diffuse[] = {1.0f, 1.0f, 1.0f, 1.0f};
  float position[] = {1.0f, 1.0f, 1.0f, 0.0f};

  glEnable(GL_LIGHT0);
  glLightfv(GL_LIGHT0, GL_AMBIENT, ambient);
  glLightfv(GL_LIGHT0, GL_DIFFUSE, diffuse);
  glLightfv(GL_LIGHT0, GL_POSITION, position);

  // Set projection matrix.
  glMatrixMode(GL_PROJECTION);
  glLoadIdentity();
  float screenW = static_cast<float>(engine->GetScreenWidth());
  float screenH = static_cast<float>(engine->GetScreenHeight());
  float aspect = screenW / screenH;
  gluPerspective(60.0f, aspect, 0.1f, 100);
  
  glMatrixMode(GL_MODELVIEW);
}

void MainState::Render() {
  if (wireframe) { glPolygonMode(GL_FRONT_AND_BACK, GL_LINE); }
  else { glPolygonMode(GL_FRONT_AND_BACK, GL_FILL); }

  glClear(GL_COLOR_BUFFER_BIT | GL_DEPTH_BUFFER_BIT);

  glLoadIdentity();

  glEnable(GL_LIGHTING);
  float ambient[] = {0.1f, 0.1f, 0.1f, 1.0f};
  float diffuse[] = {1.0f, 1.0f, 1.0f, 1.0f};
  float position[] = {0.0f, 0.0f, 0.05f, 0.0f};
  float spotDir[] = {0.0f, 0.0f, -1.0f, 0.0f};

  glEnable(GL_LIGHT0);
  glLightfv(GL_LIGHT0, GL_AMBIENT, ambient);
  glLightfv(GL_LIGHT0, GL_DIFFUSE, diffuse);
  glLightfv(GL_LIGHT0, GL_POSITION, position);
  glLightfv(GL_LIGHT0, GL_SPOT_DIRECTION, spotDir);
  glLightf(GL_LIGHT0, GL_SPOT_CUTOFF, 40);
  glLightf(GL_LIGHT0, GL_SPOT_EXPONENT, 7);

  player->SetGLCamera();
  glPushMatrix();

  

  maze->Render();

  glTranslatef(2,0,2);
  glRotatef(90,0,1,0);
  badguy->Draw();

  glPopMatrix();
  //SDL_GL_SwapBuffers();
}

void MainState::Update() {
  float fps = engine->GetFrameRate();
  if (engine->GetTicks() - lastSecond > 1000) {
    //std::cerr << "Framerate :" << fps << std::endl;
    //std::cerr << "Smallest fps:" << smallestFPS << std::endl;
    lastSecond = engine->GetTicks();
    smallestFPS = fps;
  }

  if (fps < smallestFPS) { smallestFPS = fps; }

  if (engine->IsKeyPressed(SDLK_LEFT)) {
    player->TurnLeft(engine->GetTimeDelta());
  }

  if (engine->IsKeyPressed(SDLK_RIGHT)) {
    player->TurnRight(engine->GetTimeDelta());
  }

  if (engine->IsKeyPressed(SDLK_UP)) {
    player->MoveForward(engine->GetTimeDelta());
  }

  maze->Update(engine->GetTimeDelta());

  if (gunRotLeft) {
    badguyGunLeft->rot.y -= 10.0f * engine->GetTimeDelta();
    badguyGunRight->rot.y = badguyGunLeft->rot.y;

    if (badguyGunLeft->rot.y < -10.0f) { gunRotLeft = false; }
  }
  else {
    badguyGunLeft->rot.y += 10.0f * engine->GetTimeDelta();
    badguyGunRight->rot.y = badguyGunLeft->rot.y;

    if (badguyGunLeft->rot.y > 10.0f) { gunRotLeft = true; }
  }
}

void MainState::OnKeyDown(SDL_KeyboardEvent* e) {
  if (e->keysym.sym == SDLK_F2) {
    wireframe = !wireframe;
  }
  else if (e->keysym.sym == SDLK_SPACE) {
    maze->OpenDoor();
  }
}

LevelFile* MainState::GetLevelFile() {
  return level;
}

Maze* MainState::GetMaze() {
  return maze;
}