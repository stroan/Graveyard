#ifndef WINSTATE_H
#define WINSTATE_H

#include "gamestate.h"

class Texture;

class WinState : public GameState {
private:
  Texture* splash;

  int startTime;

public:
  WinState(Engine* e);
  virtual ~WinState();

  virtual void Init();
  virtual void Resume();
  virtual void Render();

  virtual void OnKeyDown(SDL_KeyboardEvent* e);
};

#endif