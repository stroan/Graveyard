#ifndef BADGUY_H
#define BADGUY_H

class Maze;

class Badguy {
private:
  float x;
  float y;

  float orientation;

  Maze* maze;

  int state;

public:
  Badguy(Maze* m, int x, int y);

  float GetX() const;
  float GetY() const;
  float GetOrientation() const;

  void Update(float delta);
};

#endif