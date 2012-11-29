require 'sdl'
require 'opengl'
include GL

class Engine
  attr_reader :timeDelta, :frameRate, :lastTicks

  def initialize
    # Initialize SDL
    begin
      SDL.init(SDL::INIT_EVERYTHING)
      SDL.setGLAttr(SDL::GL_DOUBLEBUFFER,1)
      @screen = SDL::Screen.open(800, 600, 32, SDL::OPENGL)
      SDL::WM.set_caption("glmaze", "")
      @lastTicks = SDL.get_ticks
    rescue SDL::Error => e
      raise EngineError, "Could not initialise engine." + e.to_s
    end

    @states = []
  end

  def pushState(state)
    @states[0].pause if !@states.empty?
    @states.unshift(state)
    state.start()
    state.resume()
  end

  def popState
    if !@states.empty? then
      state = @states.shift
      state.pause()
      state.stop()
      @states[0].resume if !@states.empty?
    end
  end

  def run
    while running?
      updateTime

      SDL::Key.scan

      state = @states[0]
      state.render
      state.update

      pumpEvents
    end
  end

  def quit
    while !@states.empty? 
      popState
    end
  end

  def running?
    !@states.empty?
  end

  def screenSize
    [@screen.w, @screen.h]
  end

  private

  def pumpEvents
    while event = SDL::Event.poll
      case event
      when SDL::Event::Quit
        quit
      when SDL::Event::KeyDown
        @states[0].keyDown(event) if !@states.empty?
      end 
    end
  end

  def updateTime
    newTicks = SDL.get_ticks
    deltaTicks = newTicks - @lastTicks
    deltaTicks = 1 if deltaTicks == 0
    @timeDelta = deltaTicks.to_f / 1000.0
    @frameRate = 1.0 / @timeDelta
    @lastTicks = newTicks
  end

end

class EngineError < StandardError
end
