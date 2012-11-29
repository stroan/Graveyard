class GameState
  attr_reader :engine

  def initialize(engine)
    @engine = engine
  end

  def start
    nil
  end

  def stop
    nil
  end

  def pause
    nil
  end

  def resume
    nil
  end

  def render
    nil
  end

  def update
    nil
  end

  def keyDown(event)
    nil
  end
end
