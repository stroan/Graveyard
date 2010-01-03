#include "player.h"

#include <windows.h>
#include <GL/glu.h>
#include <cmath>

#include "levelfile.h"
#include "mainstate.h"

Player::Player(MainState* s) {
  state = s;

  LevelFile* lvl = state->GetLevelFile();

  x = lvl->GetStartX() + 0.5f;
  y = lvl->GetStartY() + 0.5f;

  orientation = 0;
}

void Player::TurnLeft(float timeDelta) {
  float speed = 3.141596f; //Measured in radians per second;
  orientation -= speed * timeDelta;
}

void Player::SetGLCamera() {
  gluLookAt(x, 0.5f, y,
            x + cos(orientation), 0.5f, y + sin(orientation),
            0, 1, 0);
}
