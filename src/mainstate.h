#ifndef MAINSTATE_H
#define MAINSTATE_H

#include "gamestate.h"
#include "player.h"

class LevelFile;
class Maze;

class MainState : public GameState {
private:
  LevelFile* level;
  Maze* maze;
  
  Player* player;

  int lastSecond;
  float smallestFPS;
  bool wireframe;
  
public:
  MainState(Engine* e);
  virtual ~MainState();

  virtual void Init();
  virtual void Resume();
  virtual void Render();
  virtual void Update();

  virtual void OnKeyDown(SDL_KeyboardEvent* e);

  LevelFile* GetLevelFile();
  Maze* GetMaze();
};

#endif
