require 'opengl'
require 'sdl'

require 'gamestate'
require 'maze'

include Gl,Glu

class MainState < GameState
  def initialize(engine)
    super(engine)

    @lastPrint = 0
  end

  def start
    @maze = Maze.new
    @maze.genGeometry
  end

  def stop
    @maze.freeGeometry
  end

  def resume
    # Set GL modes.
    glClearColor(0,0,1,1)

    # Set projection matrix.
    glMatrixMode(GL_PROJECTION)
    glLoadIdentity
    screenSize = @engine.screenSize.map {|x| x.to_f}
    aspect = screenSize[0] / screenSize[1]
    gluPerspective(90.0, aspect, 1, 100)
    
    glMatrixMode(GL_MODELVIEW)
  end

  def render
    glClear(GL_COLOR_BUFFER_BIT)
    glLoadIdentity
    gluLookAt(7,7,7, 0,0,0, 0,1,0)
    glPushMatrix

    @maze.render

    glPopMatrix
    SDL::GL.swap_buffers
  end

  def update
    if @engine.lastTicks - @lastPrint > 1000 then
      puts @engine.frameRate.to_s
      @lastPrint = @engine.lastTicks
    end
  end
end
