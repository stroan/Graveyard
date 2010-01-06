#ifndef PLAYER_H
#define PLAYER_H

class Maze;

class Player {
protected:
  float x;
  float y;

  float orientation;

  Maze* maze;

public:
  Player(Maze* m);

  void TurnLeft(float timeDelta);
  void TurnRight(float timeDelta);

  void MoveForward(float timeDelta);

  void SetGLCamera();

  float GetX() const;
  float GetY() const;
  float GetOrientation() const;
};

#endif
