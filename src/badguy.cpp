#include "badguy.h"

Badguy::Badguy(Maze* m, int tx, int ty) {
  x = (float)tx + 0.5f;
  y = (float)ty + 0.5f;
  maze = m;
  state = 0;
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

void Badguy::Update(float timeDelta) {
  if (state == 0) {
    orientation += 20.0f * timeDelta;
  }
}