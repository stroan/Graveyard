#include "sound.h"

#include <SDL_mixer.h>

Sound::Sound(Mix_Chunk* s) : sound(s) { }

void Sound::PlayEffect() { 
  Mix_PlayChannel(-1, sound, 0);
}

Sound* Sound::LoadFromFile(const std::string &sound) {
  Mix_Chunk* s = Mix_LoadWAV(sound.c_str());
  if (s) { return new Sound(s); }
  return NULL;
}