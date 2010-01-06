#include "engine.h"

#include <iostream>
#include <GL/glew.h>
#include <GL/gl.h>

#include "gamestate.h"

bool Engine::Initialize() {
  if (SDL_Init(SDL_INIT_VIDEO) != 0) { 
    std::cerr << "Error initializing SDL." << std::endl; 
    return false; 
  }
  if (SDL_GL_SetAttribute(SDL_GL_DOUBLEBUFFER,1)) { 
    std::cerr << "Error setting double buffering." << std::endl; 
    return false; 
  }
  if (!(screen = SDL_SetVideoMode(800,600,32,SDL_OPENGL))) { 
    std::cerr << "Error creating window." << std::endl; 
    return false; 
  }
  if (glewInit() != GLEW_OK) { 
    std::cerr << "Error initializing GLEW." << std::endl; 
    return false; 
  }
  SDL_WM_SetCaption("glmaze","");
  glViewport(0,0,800,600);
  lastTicks = SDL_GetTicks();
  keyState = SDL_GetKeyState(NULL);
  return true;
}

void Engine::PushState(GameState* s) {
  if (!states.empty()) { states.front()->Pause(); }
  states.push_front(s);
  s->Init();
  s->Resume();
}

void Engine::PopState() {
  if (!states.empty()) {
    GameState* s = states.front();
    states.pop_front();
    s->Pause();
    s->Term();
    if (!states.empty()) { states.front()->Resume(); }
  }
}

void Engine::Run() {
  while (IsRunning()) {
    UpdateTime();

    GameState* s = states.front();
    s->Render();
    s->Update();

    PumpEvents();

	SDL_GL_SwapBuffers();
  }
}

void Engine::Quit() {
  while (!states.empty()) {
    PopState();
  }
}

bool Engine::IsRunning() const {
  return !states.empty();
}

int Engine::GetScreenWidth() const {
  return screen->w;
}

int Engine::GetScreenHeight() const {
  return screen->h;
}

bool Engine::IsKeyPressed(int key) const {
  return keyState[key] == 1;
}

float Engine::GetTimeDelta() const {
  return timeDelta;
}

float Engine::GetTicks() const {
  return lastTicks;
}

float Engine::GetFrameRate() const {
  return frameRate;
}

void Engine::PumpEvents() {
  SDL_Event evnt;
  while (SDL_PollEvent(&evnt)) {
    switch (evnt.type) {
    case SDL_QUIT:
      Quit(); 
      break;
    case SDL_KEYDOWN:
      states.front()->OnKeyDown(&evnt.key);
      break;
    }
  }
}

void Engine::UpdateTime() {
  Uint32 newTicks = SDL_GetTicks();
  Uint32 deltaTicks = newTicks - lastTicks;
  if (deltaTicks == 0) { deltaTicks = 1; }
  lastTicks = newTicks;
  timeDelta = static_cast<float>(deltaTicks) / 1000.0f;
  frameRate = 1.0f / timeDelta;
}
