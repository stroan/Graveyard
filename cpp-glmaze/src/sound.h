#ifndef SOUND_H
#define SOUND_H

#include <string>

struct Mix_Chunk;

class Sound {
private:
  Mix_Chunk* sound;

public:
  Sound(Mix_Chunk* s);

  void PlayEffect();

  static Sound* LoadFromFile(const std::string& filename);
};

#endif