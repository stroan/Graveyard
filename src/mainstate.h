#ifndef MAINSTATE_H
#define MAINSTATE_H

#include "gamestate.h"
#include "player.h"
#include "Model_3DS.h"

class LevelFile;
class Maze;
class Sound;

class MainState : public GameState {
private:
  std::string file;

  LevelFile* level;
  Maze* maze;
  
  Player* player;

  Model_3DS* exclm;
  Model_3DS* gun;
  Model_3DS* badguy;
  Model_3DS::Object* badguyGunLeft;
  Model_3DS::Object* badguyGunRight;
  bool gunRotLeft;

  Sound* bang;
  Sound* pop;

  float bobbingexcl;
  bool bobbingup;

  int lastSecond;
  float smallestFPS;
  bool wireframe;

  bool god;
  float godx;
  float gody;
  
public:
  MainState(Engine* e, const std::string& filename);
  virtual ~MainState();

  virtual void Init();
  virtual void Term();
  virtual void Resume();
  virtual void Render();
  virtual void Update();

  virtual void OnKeyDown(SDL_KeyboardEvent* e);

  LevelFile* GetLevelFile();
  Maze* GetMaze();
};

#endif
