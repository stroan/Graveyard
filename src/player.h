#ifndef PLAYER_H
#define PLAYER_H

class Maze;

class Player {
protected:
  float x;
  float y;

  float orientation;

  Maze* maze;

  bool dead;

public:
  Player(Maze* m);

  void TurnLeft(float timeDelta);
  void TurnRight(float timeDelta);

  void MoveForward(float timeDelta);
  void MoveBackward(float timeDelta);

  void Kill();

  void SetGLCamera();

  float GetX() const;
  float GetY() const;
  float GetOrientation() const;
  bool IsDead() const;
};

#endif
