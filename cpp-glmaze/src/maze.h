#ifndef MAZE_H
#define MAZE_H

#include <GL/glew.h>
#include <GL/gl.h>
#include <vector>

class LevelFile;
class Texture;
class Door;
class Player;
class Badguy;

class Maze {
private: 
  LevelFile* level;

  GLuint wallsVBO;
  int numTris;
  GLuint doorVBO;
  int numDoorTris;
  GLuint floorVBO;

  Texture* wallTex;
  Texture* doorTex;
  Texture* floorTex;

  // Allow for spacial lookup of doors, 
  // and fast interation of all doors.
  Door** doorGrid;
  std::vector<Door*> doors;

  std::vector<Badguy*> badguys;

  Player* player;

public:
  Maze(LevelFile* lvl);
  ~Maze();

  void InitGeometry();

  void Render();
  void Update(float timeDelta);

  void OpenDoor();
  bool Shoot();

  bool CanMoveTo(float x, float y) const;
  bool IsPassable(int x, int y) const;

  int GetNumBadguys() const;
  Badguy* GetBadguy(int i) const;

  Player* GetPlayer() const;
  LevelFile* GetLevelFile() const;
};

#endif
