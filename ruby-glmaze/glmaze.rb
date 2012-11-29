#!/usr/bin/ruby

require 'engine'
require 'mainstate'

engine = Engine.new
mainState = MainState.new(engine)
engine.pushState(mainState)
engine.run
