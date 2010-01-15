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
#include "badguy.h"
#include "winstate.h"

#include "Model_3DS.h"

MainState::MainState(Engine* e, const std::string& filename) : GameState(e), file(filename) { }
MainState::~MainState() { }

void MainState::Init() {
  level = LevelFile::Load("data/levels/" + file);

  maze = new Maze(level);
  maze->InitGeometry();

  player = maze->GetPlayer();

  exclm = new Model_3DS();
  exclm->Load("data/models/exclm.3DS");
  exclm->scale = 0.01f;

  gun = new Model_3DS();
  gun->Load("data/models/gun.3DS");
  gun->scale = 0.01f;

  bobbingexcl = 0.0f;
  bobbingup = true;

  badguy = new Model_3DS();
  badguy->Load("data/models/badguy.3DS");
  badguy->scale = 0.015f;

  badguyGunLeft = badguy->GetObjectByName("LeftGun");
  badguyGunRight = badguy->GetObjectByName("RightGun");
  gunRotLeft = true;

  lastSecond = 0;
  smallestFPS = 0;
  wireframe = false;
  god = false;
  godx = gody = 0;
}

void MainState::Term() {
  delete level;
  delete maze;
  delete exclm;
  delete gun;
  delete badguy;
}

void MainState::Resume() {
  glClearColor(0,0.1,0.1,1);
  glClearDepth(1);

  glDisable(GL_CULL_FACE);
  glEnable(GL_DEPTH_TEST);

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
  float ambient[] = {1.95f, 1.95f, 1.95f, 1.0f};
  float diffuse[] = {1.0f, 1.0f, 1.0f, 1.0f};
  float position[] = {0.0f, 0.0f, 0.05f, 0.0f};

  glEnable(GL_LIGHT0);
  glLightfv(GL_LIGHT0, GL_AMBIENT, ambient);
  glLightfv(GL_LIGHT0, GL_DIFFUSE, diffuse);
  glLightfv(GL_LIGHT0, GL_POSITION, position);

  if (!god) {
    glPushMatrix();
    glTranslatef(-0.25f,-0.275,-0.2);
    glRotatef(180,0,1,0);
    gun->Draw();
    glPopMatrix();

    player->SetGLCamera();
  }
  else {
    gluLookAt((level->GetGridWidth() / 2.0f + 0.5f) + godx, 5.0f, (level->GetGridHeight() / 2.0f + 0.5f) + gody,
              (level->GetGridWidth() / 2.0f + 0.5f) + godx, 0.0f, (level->GetGridHeight() / 2.0f + 0.5f) + gody,
              0,0,1);
  }

  glPushMatrix();

  

  maze->Render();

  glPushMatrix();
  glTranslatef(level->GetEndX() + 0.5f, 0.3f + bobbingexcl, level->GetEndY() + 0.5f);
  exclm->Draw();
  glPopMatrix();

  int numBadguys = maze->GetNumBadguys();
  for (int i = 0; i < numBadguys; i++) {
    Badguy* b = maze->GetBadguy(i);
    if (b->IsDead()) { continue; }
    glPushMatrix();
    glTranslatef(b->GetX(), 0, b->GetY());
    glRotatef((b->GetOrientation() / 3.141596f) * 180.0f, 0, 1, 0);
    badguy->Draw();
    glPopMatrix();
  }

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
    if (!god) {
      player->TurnLeft(engine->GetTimeDelta());
    }
    else {
      godx += 5 * engine->GetTimeDelta();
    }
  }

  if (engine->IsKeyPressed(SDLK_RIGHT)) {
    if (!god) {
      player->TurnRight(engine->GetTimeDelta());
    }
    else {
      godx -= 5 * engine->GetTimeDelta();
    }
  }

  if (engine->IsKeyPressed(SDLK_UP)) {
    if (!god) {
      player->MoveForward(engine->GetTimeDelta());
    }
    else {
      gody += 5 * engine->GetTimeDelta();
    }
  }

  if (engine->IsKeyPressed(SDLK_DOWN)) {
    if (!god) {
      player->MoveBackward(engine->GetTimeDelta());
    }
    else {
      gody -= 5 * engine->GetTimeDelta();
    }
  }


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

  if (bobbingup) {
    bobbingexcl += engine->GetTimeDelta();
    if (bobbingexcl > 0.2f) { bobbingup = false; } 
  }
  else {
    bobbingexcl -= engine->GetTimeDelta();
    if (bobbingexcl < -0.2f) { bobbingup = true; } 
  }

  maze->Update(engine->GetTimeDelta());

  if (player->IsDead()) {
    engine->PopState();
    engine->PushState(new MainState(engine, file));
    return;
  }

  float dx = player->GetX() - (level->GetEndX() + 0.5f);
  float dy = player->GetY() - (level->GetEndY() + 0.5f);
  if ((dx * dx) + (dy * dy) < 1) {
    engine->PopState();
    if (level->GetNextLevel().compare("NONE") != 0) {
      engine->PushState(new MainState(engine, level->GetNextLevel()));
    }
    else {
      engine->PushState(new WinState(engine));
    }
  }
}

void MainState::OnKeyDown(SDL_KeyboardEvent* e) {
  if (e->keysym.sym == SDLK_F2) {
    wireframe = !wireframe;
  }
  else if (e->keysym.sym == SDLK_e && !god) {
    maze->OpenDoor();
  }
  else if (e->keysym.sym == SDLK_SPACE && !god) {
    maze->Shoot();
  }
  else if (e->keysym.sym == SDLK_F3) {
    god = !god;
  }
}

LevelFile* MainState::GetLevelFile() {
  return level;
}

Maze* MainState::GetMaze() {
  return maze;
}
