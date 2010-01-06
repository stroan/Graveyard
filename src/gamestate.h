#ifndef GAMESTATE_H
#define GAMESTATE_H

#include <SDL.h>

class Engine;

class GameState {
protected:
  Engine* engine;

public:
  GameState(Engine* e);
  virtual ~GameState();

  virtual void Init();
  virtual void Term();

  virtual void Pause();
  virtual void Resume();

  virtual void Render();
  virtual void Update();

  virtual void OnKeyDown(SDL_KeyboardEvent* e);
};

#endif
