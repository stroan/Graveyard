#include "maze.h"

#include <boost/foreach.hpp>
#include <cmath>
#include <iostream>
#include <memory.h>

#include "levelfile.h"
#include "texture.h"
#include "vertex.h"
#include "tile.h"
#include "player.h"
#include "badguy.h"

Maze::Maze(LevelFile* lvl) {
  level = lvl;

  doorGrid = new Door*[lvl->GetGridWidth() * lvl->GetGridHeight()];
  memset(doorGrid, 0, sizeof(Door*) * lvl->GetGridWidth() * lvl->GetGridHeight());

  for (int row = 1; row < level->GetGridHeight() - 1; row++) {
    for (int col = 1; col < level->GetGridWidth() - 1; col++) {
      int doorType = level->IsDoor(col, row);
      if (!doorType) { continue; }

      Door* p = new Door(doorType == 2, col, row);
      doors.push_back(p);
      doorGrid[row * level->GetGridWidth() + col] = p;
    }
  }

  for (int i = 0; i < level->GetNumBadguys(); i++) {
    LevelFile::BadguyPos pos = level->GetBadguy(i);
    badguys.push_back(new Badguy(this, pos.x, pos.y));
  }

  player = new Player(this);
}

Maze::~Maze() {
  delete doorGrid;
}

void Maze::InitGeometry() {
  // *********************************************************************
  // Make walls

  int w = level->GetGridWidth();
  int h = level->GetGridHeight();

  numTris = 0;

  float wallHeight = 1;

  std::vector<VertPosTexNorm> v;

  for (int row = 1; row < h - 1; row++) {
    int startTop = -1;
    int startBottom = -1;
    for (int col = 1; col < w; col++) {
      bool thisCell = level->IsWall(col,row);
      bool upCell = level->IsWall(col,row-1);
      bool downCell = level->IsWall(col,row+1);

      if (upCell && !thisCell && startTop == -1) { startTop = col; }
      if (downCell && !thisCell && startBottom == -1) { startBottom = col; }

      if (startTop != -1 && (thisCell|| !upCell)) {
        v.push_back(VertPosTexNorm(col, wallHeight, row,        (col - startTop), 1,    0,0,1));
        v.push_back(VertPosTexNorm(startTop, 0, row,            0, 0,                   0,0,1));
        v.push_back(VertPosTexNorm(startTop, wallHeight, row,   0, 1,                   0,0,1));

        v.push_back(VertPosTexNorm(col, wallHeight, row,        (col - startTop), 1,    0,0,1));
        v.push_back(VertPosTexNorm(col, 0, row,                 (col - startTop), 0,    0,0,1));
        v.push_back(VertPosTexNorm(startTop, 0, row,            0, 0,                   0,0,1));
    
        numTris += 2;
        startTop = -1;
      }

      if (startBottom != -1 && (thisCell || !downCell)) {
        v.push_back(VertPosTexNorm(col, wallHeight, row + 1,          (col - startBottom), 1,       0,0,-1));
        v.push_back(VertPosTexNorm(startBottom, 0, row + 1,           0,0,                          0,0,-1));
        v.push_back(VertPosTexNorm(startBottom, wallHeight, row + 1,  0, 1,                         0,0,-1));

        v.push_back(VertPosTexNorm(col, wallHeight, row + 1,          (col - startBottom), 1,       0,0,-1));
        v.push_back(VertPosTexNorm(col, 0, row + 1,                   (col - startBottom), 0,       0,0,-1));
        v.push_back(VertPosTexNorm(startBottom, 0, row + 1,           0, 0,                         0,0,-1));
    
        numTris += 2;
        startBottom = -1;
      }
    }
  }

  for (int col = 1; col < w - 1; col++) {
    int startLeft = -1;
    int startRight = -1;
    for (int row = 1; row < h; row++) {
      bool thisCell = level->IsWall(col,row);
      bool leftCell = level->IsWall(col - 1,row);
      bool rightCell = level->IsWall(col + 1,row);

      if (leftCell && !thisCell && startLeft == -1) { startLeft = row; }
      if (rightCell && !thisCell && startRight == -1) { startRight = row; }

      if (startLeft != -1 && (thisCell|| !leftCell)) {
        v.push_back(VertPosTexNorm(col, wallHeight, startLeft,    0, 1,                     1,0,0));
        v.push_back(VertPosTexNorm(col, 0, startLeft,             0, 0,                     1,0,0));
        v.push_back(VertPosTexNorm(col, wallHeight, row,          (startLeft - row), 1,     1,0,0));

        v.push_back(VertPosTexNorm(col, wallHeight, row,          (startLeft - row), 1,         1,0,0));
        v.push_back(VertPosTexNorm(col, 0, startLeft,             0, 0,                         1,0,0));
        v.push_back(VertPosTexNorm(col, 0, row,                   (startLeft - row), 0,         1,0,0));
    
        numTris += 2;
        startLeft = -1;
      }

      if (startRight != -1 && (thisCell || !rightCell)) {
        v.push_back(VertPosTexNorm(col + 1, wallHeight, startRight,   0, 1,                     -1,0,0));
        v.push_back(VertPosTexNorm(col + 1, 0, startRight,            0, 0,                     -1,0,0));
        v.push_back(VertPosTexNorm(col + 1, wallHeight, row,          (startRight - row), 1,    -1,0,0));

        v.push_back(VertPosTexNorm(col + 1, wallHeight, row,          (startRight - row), 1,    -1,0,0));
        v.push_back(VertPosTexNorm(col + 1, 0, startRight,            0, 0,                     -1,0,0));
        v.push_back(VertPosTexNorm(col + 1, 0, row,                   (startRight - row), 0,    -1,0,0));
    
        numTris += 2;
        startRight = -1;
      }
    }
  }

  glGenBuffers(1,&wallsVBO);
  glBindBuffer(GL_ARRAY_BUFFER, wallsVBO);
  VertPosTexNorm::FillBuffer(v);
  glBindBuffer(GL_ARRAY_BUFFER, 0);

  wallTex = Texture::LoadFromFile("data/textures/wall.bmp");

  // *************************************************************************
  // Make door
  
  std::vector<VertPosTexNorm> doorV;

  doorV.push_back(VertPosTexNorm(-0.5f, 1, -0.25f,     0,1,       0,0,-1));
  doorV.push_back(VertPosTexNorm(-0.5f, 0, -0.25f,     0,0,       0,0,-1));
  doorV.push_back(VertPosTexNorm(0.5f, 0, -0.25f,     0.66f,0,    0,0,-1));

  doorV.push_back(VertPosTexNorm(-0.5f, 1, -0.25f,     0,1,       0,0,-1));
  doorV.push_back(VertPosTexNorm(0.5f, 0, -0.25f,     0.66f,0,    0,0,-1));
  doorV.push_back(VertPosTexNorm(0.5f, 1, -0.25f,     0.66f,1,    0,0,-1));

  doorV.push_back(VertPosTexNorm(-0.5f, 1, 0.25f,     0,1,        0,0,1));
  doorV.push_back(VertPosTexNorm(-0.5f, 0, 0.25f,     0,0,        0,0,1));
  doorV.push_back(VertPosTexNorm(0.5f, 0, 0.25f,     0.66f,0,     0,0,1));

  doorV.push_back(VertPosTexNorm(-0.5f, 1, 0.25f,     0,1,        0,0,1));
  doorV.push_back(VertPosTexNorm(0.5f, 0, 0.25f,     0.66f,0,     0,0,1));
  doorV.push_back(VertPosTexNorm(0.5f, 1, 0.25f,     0.66f,1,     0,0,1));

  doorV.push_back(VertPosTexNorm(-0.5f, 1, -0.25f,     0.66f,1,   -1,0,0));
  doorV.push_back(VertPosTexNorm(-0.5f, 1, 0.25f,      1,1,       -1,0,0));
  doorV.push_back(VertPosTexNorm(-0.5f, 0, 0.25f,      1,0,       -1,0,0));

  doorV.push_back(VertPosTexNorm(-0.5f, 1, -0.25f,     0.66f,1,   -1,0,0));
  doorV.push_back(VertPosTexNorm(-0.5f, 0, 0.25f,      1,0,       -1,0,0));
  doorV.push_back(VertPosTexNorm(-0.5f, 0, -0.25f,     0.66f,0,   -1,0,0));

  numDoorTris = 6;

  glGenBuffers(1,&doorVBO);
  glBindBuffer(GL_ARRAY_BUFFER, doorVBO);
  VertPosTexNorm::FillBuffer(doorV);
  glBindBuffer(GL_ARRAY_BUFFER, 0);

  doorTex = Texture::LoadFromFile("data/textures/door.bmp");
}

void Maze::Render() {
  glEnable(GL_TEXTURE_2D);
  wallTex->Bind();
  glBindBuffer(GL_ARRAY_BUFFER, wallsVBO);
  VertPosTexNorm::SetGLModes();
  glDrawArrays(GL_TRIANGLES, 0, numTris * 3);

  doorTex->Bind();
  glBindBuffer(GL_ARRAY_BUFFER, doorVBO);
  VertPosTexNorm::SetGLModes();
  BOOST_FOREACH(Door* p, doors) {
    glPushMatrix();
    p->SetTransform();
    glDrawArrays(GL_TRIANGLES, 0, numDoorTris * 3);
    glPopMatrix();
  }
}

void Maze::Update(float timeDelta) {
  BOOST_FOREACH(Door* d, doors) {
    d->Update(timeDelta);
  }

  BOOST_FOREACH(Badguy* b, badguys) {
    b->Update(timeDelta);

    float dx = b->GetX() - player->GetX();
    float dy = b->GetY() - player->GetY();
    if ((dx * dx) + (dy * dy) < 1 && !b->IsDead()) { 
      player->Kill();
    }
  }
}

void Maze::OpenDoor() {
  float orientation = player->GetOrientation();
  int playerX = player->GetX();
  int playerY = player->GetY();
  int openTileX = static_cast<int>(player->GetX() + (cos(orientation) * 0.5f));
  int openTileY = static_cast<int>(player->GetY() - (sin(orientation) * 0.5f));
  int w = level->GetGridWidth();

  std::cerr << "Player: " << playerX << "," << playerY << "\n";
  std::cerr << "Open: " << openTileX << "," << openTileY << "\n";

  Door* door = doorGrid[openTileY * w + openTileX];
  if (door) { door->Open(); }
}

bool Maze::CanMoveTo(float x, float y) const {
  int tileX = static_cast<float>(x);
  int tileY = static_cast<float>(y);
  float dx = x - static_cast<float>(tileX);
  float dy = y - static_cast<float>(tileY);
  if (dx < 0.15f) { tileX--; }
  if (dx > 0.85f) { tileX++; }
  if (dy < 0.15f) { tileY--; }
  if (dy > 0.85f) { tileY++; }

  return IsPassable(tileX, tileY);
}

void Maze::Shoot() {
  BOOST_FOREACH(Badguy* b, badguys) {
    float dx = b->GetX() - player->GetX();
    float dy = player->GetY() - b->GetY();

    float angle = atan(dy / dx);
    if (dx < 0) { angle = 3.14159f + angle; }

    float facingAngle = player->GetOrientation() - angle;
    while (facingAngle < 0) { facingAngle += 3.141596f * 2.0f; }
    while (facingAngle > 3.141596f * 2.0f) { facingAngle -= 3.141596f * 2.0f; }

    bool hit = false;
    if (facingAngle < 0.3f) { hit = true; }
    if (facingAngle > 3.141596f * 2.0f - 0.3f) { hit = true; }

    float length = sqrt((dx * dx) + (dy * dy));
    for (float d = 0; d < length && hit; d++) {
      float nx = player->GetX() + cos(player->GetOrientation()) * d;
      float ny = player->GetY() - sin(player->GetOrientation()) * d;

      if (!IsPassable(nx,ny)) { hit = false; }
    }

    if (hit) { b->Kill(); }
  }
}

bool Maze::IsPassable(int x, int y) const {
  int w = level->GetGridWidth();

  Door* door = doorGrid[y * w + x];
  if (door && door->IsClosed()) { 
    return false; 
  }

  return !level->IsWall(x, y);
}


int Maze::GetNumBadguys() const {
  return badguys.size();
}

Badguy* Maze::GetBadguy(int i) const {
  return badguys[i];
}

Player* Maze::GetPlayer() const {
  return player;
}

LevelFile* Maze::GetLevelFile() const {
  return level;
}
