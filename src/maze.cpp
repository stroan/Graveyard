#include "maze.h"

#include "levelfile.h"
#include "vertex.h"

Maze::Maze(LevelFile* lvl) {
  level = lvl;
}

Maze::~Maze() { }

void Maze::InitGeometry() {
  int w = level->GetGridWidth();
  int h = level->GetGridHeight();

  numTris = 0;

  float wallHeight = 1;

  std::vector<VertPosCol> v;

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
        v.push_back(VertPosCol(col, wallHeight, row, 1, 0, 0));
        v.push_back(VertPosCol(startTop, 0, row, 1,0,0));
        v.push_back(VertPosCol(startTop, wallHeight, row, 1, 0, 0));

        v.push_back(VertPosCol(col, wallHeight, row, 0, 1, 0));
        v.push_back(VertPosCol(col, 0, row, 0, 1, 0));
        v.push_back(VertPosCol(startTop, 0, row, 0, 1, 0));
    
        numTris += 2;
        startTop = -1;
      }

      if (startBottom != -1 && (thisCell || !downCell)) {
        v.push_back(VertPosCol(col, wallHeight, row + 1, 1, 0, 0));
        v.push_back(VertPosCol(startBottom, 0, row + 1, 1,0,0));
        v.push_back(VertPosCol(startBottom, wallHeight, row + 1, 1, 0, 0));

        v.push_back(VertPosCol(col, wallHeight, row + 1, 0, 1, 0));
        v.push_back(VertPosCol(col, 0, row + 1, 0, 1, 0));
        v.push_back(VertPosCol(startBottom, 0, row + 1, 0, 1, 0));
    
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
        v.push_back(VertPosCol(col, wallHeight, startLeft, 1, 0, 0));
        v.push_back(VertPosCol(col, 0, startLeft, 1,0,0));
        v.push_back(VertPosCol(col, wallHeight, row, 1, 0, 0));

        v.push_back(VertPosCol(col, wallHeight, row, 0, 1, 0));
        v.push_back(VertPosCol(col, 0, startLeft, 0, 1, 0));
        v.push_back(VertPosCol(col, 0, row, 0, 1, 0));
    
        numTris += 2;
        startLeft = -1;
      }

      if (startRight != -1 && (thisCell || !rightCell)) {
        v.push_back(VertPosCol(col + 1, wallHeight, startRight, 1, 0, 0));
        v.push_back(VertPosCol(col + 1, 0, startRight, 1,0,0));
        v.push_back(VertPosCol(col + 1, wallHeight, row, 1, 0, 0));

        v.push_back(VertPosCol(col + 1, wallHeight, row, 0, 1, 0));
        v.push_back(VertPosCol(col + 1, 0, startRight, 0, 1, 0));
        v.push_back(VertPosCol(col + 1, 0, row, 0, 1, 0));
    
        numTris += 2;
        startRight = -1;
      }
    }
  }

  glGenBuffers(1,&wallsVBO);
  glBindBuffer(GL_ARRAY_BUFFER, wallsVBO);
  VertPosCol::FillBuffer(v);
}

void Maze::Render() {
  VertPosCol::SetGLModes();
  glBindBuffer(GL_ARRAY_BUFFER, wallsVBO);
  glDrawArrays(GL_TRIANGLES, 0, numTris * 3);
}
