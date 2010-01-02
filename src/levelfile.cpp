#include "levelfile.h"

#include <boost/algorithm/string.hpp>
#include <boost/foreach.hpp>
#include <boost/lexical_cast.hpp>
#include <iostream>
#include <fstream>
#include <sstream>

int LevelFile::GetGridWidth() const {
  return gridWidth;
}

int LevelFile::GetGridHeight() const {
  return gridHeight;
}

bool LevelFile::IsWall(int x, int y) const {
  return grid[y * gridWidth + x] == 1;
}

int LevelFile::GetStartX() const {
  return startX;
}

int LevelFile::GetStartY() const {
  return startY;
}

LevelFile* LevelFile::Load(const std::string& filename) {
  std::ifstream inFile(filename.c_str()); 
  if (!inFile) { 
    std::cerr << "Could not load leve file: " + filename << std::endl;
    return NULL; 
  }

  LevelFile* retVal = new LevelFile();

  while(!inFile.eof()) {
    std::string line;
    std::getline(inFile, line);
    if(line.find("GRID:") != std::string::npos) {
      std::getline(inFile, line);
      std::vector<std::string> strs;
      boost::split(strs, line, boost::is_any_of(","));
      
      int w = boost::lexical_cast<int>(strs[0]);
      int h = boost::lexical_cast<int>(strs[1]);

      for (int y = 0; y < h; y++) {
        std::getline(inFile, line);
        boost::split(strs, line, boost::is_any_of(","));
        BOOST_FOREACH( std::string cell, strs ) {
          retVal->grid.push_back(static_cast<char>(boost::lexical_cast<int>(cell)));
        }
      }

      retVal->gridWidth = w;
      retVal->gridHeight = h;
    }

    else if (line.find("START:") != std::string::npos) {
      std::getline(inFile, line);
      std::vector<std::string> strs;
      boost::split(strs, line, boost::is_any_of(","));
      
      int x = boost::lexical_cast<int>(strs[0]);
      int y = boost::lexical_cast<int>(strs[1]);

      retVal->startX = x;
      retVal->startY = y;
    }
  }
 
  return retVal;
}
