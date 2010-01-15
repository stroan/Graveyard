#ifndef BADGUY_H
#define BADGUY_H

class Maze;

class Badguy {
private:
  float x;
  float y;

  float destx;
  float desty;

  float orientation;

  Maze* maze;

  int state;

public:
  Badguy(Maze* m, int x, int y);

  float GetX() const;
  float GetY() const;
  float GetOrientation() const;
  bool IsDead() const;

  void Update(float delta);
  void Kill();
};

#endif