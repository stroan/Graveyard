#ifndef PLAYER_H
#define PLAYER_H

class MainState;

class Player {
protected:
  float x;
  float y;

  float orientation;

  MainState* state;

public:
  Player(MainState* s);

  void TurnLeft(float timeDelta);

  void SetGLCamera();
};

#endif
