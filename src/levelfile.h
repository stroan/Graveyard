#ifndef LEVELFILE_H
#define LEVELFILE_H

#include <string>
#include <vector>

class LevelFile {
private:
  std::vector<char> grid;
  int gridWidth;
  int gridHeight;

  int startX;
  int startY;

public:
  int GetGridWidth() const;
  int GetGridHeight() const;
  bool IsWall(int x, int y) const;
  int IsDoor(int x, int y) const;

  int GetStartX() const;
  int GetStartY() const;

  static LevelFile* Load(const std::string& file);
};

#endif
