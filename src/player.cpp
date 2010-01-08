#include "player.h"

#include <GL/glew.h>
#include <GL/glu.h>
#include <cmath>

#include "levelfile.h"
#include "maze.h"

Player::Player(Maze* m) {
  maze = m;

  LevelFile* lvl = maze->GetLevelFile();

  x = lvl->GetStartX() + 0.5f;
  y = lvl->GetStartY() + 0.5f;

  orientation = 0;
}

void Player::TurnLeft(float timeDelta) {
  float speed = 3.141596f; //Measured in radians per second;
  orientation -= speed * timeDelta;
}

void Player::TurnRight(float timeDelta) {
  float speed = 3.141596f;
  orientation += speed * timeDelta;
}

void Player::MoveForward(float timeDelta) {
  float speed = 1; // Measured in distance units per second;
  float nx = x + cos(orientation) * speed * timeDelta;
  float ny = y + sin(orientation) * speed * timeDelta;
  if (maze->CanMoveTo(nx,ny)) { x = nx; y = ny; }
}

void Player::SetGLCamera() {
  gluLookAt(x, 0.55f, y,
            x + cos(orientation), 0.55f, y + sin(orientation),
            0, 1, 0);
}

float Player::GetX() const {
  return x;
}

float Player::GetY() const {
  return y;
}

float Player::GetOrientation() const {
  return orientation;
}