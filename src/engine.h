#ifndef ENGINE_H
#define ENGINE_H

#include <SDL.h>
#include <list>

class GameState;

class Engine {
private:
  SDL_Surface* screen;
  Uint32 lastTicks;
  float timeDelta;
  float frameRate;

  Uint8* keyState;

  typedef std::list<GameState*> StateStack;
  StateStack states;

public:
  bool Initialize();

  void PushState(GameState* g);
  void PopState();

  void Run();
  void Quit();

  bool IsRunning() const;
  int GetScreenWidth() const;
  int GetScreenHeight() const;
 
  bool IsKeyPressed(int key) const;
  
  float GetTimeDelta() const;
  float GetTicks() const;
  float GetFrameRate() const;

private:
  void PumpEvents();
  void UpdateTime();
};

#endif
