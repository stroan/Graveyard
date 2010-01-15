#include "badguy.h"

#include <cmath>
#include <cstdlib>

#include "maze.h"

Badguy::Badguy(Maze* m, int tx, int ty) {
  x = destx = (float)tx + 0.5f;
  y = desty = (float)ty + 0.5f;
  maze = m;
  state = 1;
  orientation = 0;
}

float Badguy::GetX() const {
  return x;
}

float Badguy::GetY() const {
  return y;
}

float Badguy::GetOrientation() const {
  return orientation;
}

bool Badguy::IsDead() const {
  return state == -1;
}

void Badguy::Update(float timeDelta) {
  if (state == 0) {   // Rotate till we're facing in the right direction
    orientation += 10.0f * timeDelta;

    float dx = destx - x;
    float dy = y - desty;

    float angle = atan(dy / dx);
    if (dx < 0) { angle = 3.14159f + angle; }


    float facingAngle = orientation - angle;
    while (facingAngle < 0) { facingAngle += 3.141596f * 2.0f; }
    while (facingAngle > 3.141596f * 2.0f) { facingAngle -= 3.141596f * 2.0f; }

    bool hit = false;
    if (facingAngle < 0.2f) { hit = true; }
    if (facingAngle > 3.141596f * 2.0f - 0.2f) { hit = true; }

    if (hit) {
      orientation = angle;
      state = 1;
    }
  }
  else if (state == 1) {  // Move in the right direction
    float dx = destx - x;
    float dy = y - desty;

    float dist1 = (dx * dx) + (dy * dy);

    x = x + (cos(orientation) * timeDelta * 5.0f);
    y = y - (sin(orientation) * timeDelta * 5.0f);

    dx = destx - x;
    dy = y - desty;

    float dist2 = (dx * dx) + (dy * dy);
    if (dist2 > dist1) {
      x = destx;
      y = desty;

      bool done = false;
      int i = rand() % 4;
      while (!done) {
        int nx, ny;
        if (i == 0) { nx = x + 1; ny = y; }
        else if (i == 1) { nx = x - 1; ny = y; }
        else if (i == 2) { nx = x; ny = y + 1; }
        else if (i == 3) { nx = x; ny = y - 1; }

        if (maze->IsPassable(nx,ny)) { 
          destx = nx + 0.5f;
          desty = ny + 0.5f;
          done = true;
        }
        else { i = (i + 1) % 4; }
      }
      

      state = 0;
    }
  }
}

void Badguy::Kill() {
  state = -1;
}