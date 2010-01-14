#ifndef LEVELFILE_H
#define LEVELFILE_H

#include <string>
#include <vector>

class LevelFile {
public:
  struct BadguyPos {
    int x;
    int y;
  };

private:
  std::vector<char> grid;
  int gridWidth;
  int gridHeight;

  int startX;
  int startY;

  int endX;
  int endY;

  std::vector<BadguyPos> badguys;

public:
  int GetGridWidth() const;
  int GetGridHeight() const;
  bool IsWall(int x, int y) const;
  int IsDoor(int x, int y) const;

  int GetStartX() const;
  int GetStartY() const;

  int GetEndX() const;
  int GetEndY() const;

  int GetNumBadguys() const;
  BadguyPos GetBadguy(int i) const;

  static LevelFile* Load(const std::string& file);
};

#endif
