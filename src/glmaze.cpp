#include "engine.h"
#include "mainstate.h"

int main(int argn, char** argv) {
  Engine* e = new Engine();
  MainState* s = new MainState(e);

  if (!e->Initialize()) { return 1; }

  e->PushState(s);
  e->Run();
  return 0;
}
