#ifndef MAZE_H
#define MAZE_H

#include <GL/glew.h>
#include <GL/gl.h>

class LevelFile;

class Maze {
private: 
  GLuint wallsVBO;
  LevelFile* level;
  int numTris;

public:
  Maze(LevelFile* lvl);
  ~Maze();

  void InitGeometry();

  void Render();
};

#endif
