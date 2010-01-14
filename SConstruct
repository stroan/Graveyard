import glob

env = Environment()

env.ParseConfig('sdl-config --cflags')
env.ParseConfig('sdl-config --libs')
env.Append(LIBS = ['GL','GLU','GLEW'])
env.Append(CCFLAGS = ' -g -rdynamic')
env.Append(LINKFLAGS = ' -g -rdynamic')

SOURCES = glob.glob('src/*.cpp')
LIBS = ['gl'];

env.Program('glmaze', SOURCES)

