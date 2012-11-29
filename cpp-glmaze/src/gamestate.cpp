#include "gamestate.h"

GameState::GameState(Engine* e) : engine(e) { }
GameState::~GameState() { }
void GameState::Init() { }
void GameState::Term() { }
void GameState::Pause() { }
void GameState::Resume() { }
void GameState::Render() { }
void GameState::Update() { }
void GameState::OnKeyDown(SDL_KeyboardEvent *e) { }
