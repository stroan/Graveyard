require 'opengl'
require 'vertex'

class Maze
  def genGeometry
    v = Vertex.new(pos = [0,0,0], color = [0,0,0])
    v2 = v.toArray(pos = 3, color = 3).pack("f*")

    geom = [0,0,0, 0,0,0].pack("f*")
    @vbuffer = glGenBuffers(1)
    glBindBuffer(GL_ARRAY_BUFFER, @vbuffer[0])
    glBufferData(GL_ARRAY_BUFFER, 4 * v2.length, v2, GL_STATIC_DRAW)
  end

  def freeGeometry
    glDeleteBuffers(@vbuffer)
  end

  def render
    Vertex.setGLModes(pos = 3, color = 3)
    glBindBuffer(GL_ARRAY_BUFFER, @vbuffer[0])
    glDrawArrays(GL_POINTS,0,1)
  end
end
