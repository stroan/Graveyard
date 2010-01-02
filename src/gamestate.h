#ifndef GAMESTATE_H
#define GAMESTATE_H

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
};

#endif
