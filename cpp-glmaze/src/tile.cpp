#include "tile.h"

#include <GL/glew.h>
#include <GL/gl.h>

#include "maze.h"
#include "player.h"

Door::Door(Maze* m, bool horiz, int x, int y) {
  horizontal = horiz;
  tileX = x;
  tileY = y;
  maze = m;

  closed = true;
  offset = 0;
}

void Door::Open() {
  closed = false;
  countdown = 3.0f;
}

void Door::Update(float timeDelta) {
  if (!closed && offset < 0.9f) {
    offset += 1 * timeDelta;
  }

  if (!closed && offset >= 0.9f) {
    countdown -= timeDelta;
    if (countdown < 0) {
      if ((int)maze->GetPlayer()->GetX() == tileX && (int)maze->GetPlayer()->GetY() == tileY) {
        countdown = 3;
      }
      else {
        closed = true;
      }
    }
  }

  if (closed && offset > 0) {
    offset -= 1 * timeDelta;
    if (offset < 0) { offset = 0; }
  }
}

void Door::SetTransform() {
  glTranslatef(tileX + 0.5, 0, tileY + 0.5f);
  if (!horizontal) { 
    glRotatef(90, 0, 1, 0); 
  }
  glTranslatef(offset, 0, 0);
}

bool Door::IsClosed() const {
  return closed;
}

bool Door::IsHorizontal() const {
  return horizontal;
}

int Door::GetX() const {
  return tileX;
}

int Door::GetY() const {
  return tileY;
}