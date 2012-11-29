#include "engine.h"
#include "startstate.h"

int main(int argn, char** argv) {
  Engine* e = new Engine();
  StartState* s = new StartState(e);

  if (!e->Initialize()) { return 1; }

  e->PushState(s);
  e->Run();
  return 0;
}
