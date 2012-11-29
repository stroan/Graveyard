#ifndef STARTSTATE_H
#define STARTSTATE_H

#include "gamestate.h"

class Texture;

class StartState : public GameState {
private:
  Texture* splash;

public:
  StartState(Engine* e);
  virtual ~StartState();

  virtual void Init();
  virtual void Resume();
  virtual void Render();

  virtual void OnKeyDown(SDL_KeyboardEvent* e);
};

#endif