#ifndef TILE_H
#define TILE_H

class Door {
private:
  bool horizontal;
  int tileX;
  int tileY;

  bool closed;
  float offset;
  float countdown;

public:
  Door(bool horizontal, int x, int y);

  void Open();
  void Update(float timeDelta);

  void SetTransform();

  bool IsClosed() const;
  bool IsHorizontal() const;
  int GetX() const;
  int GetY() const;
};

#endif