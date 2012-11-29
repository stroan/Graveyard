class Vertex
  attr_reader :pos, :color, :normal
  attr_writer :pos, :color, :normal
  
  def initialize(pos = nil, color = nil, normal = nil)
    @pos = pos
    @color = color
    @normal = normal
  end  

  def toArray(pos = nil, color = nil, normal = nil)
    result = []
    expectedSize = pos.to_i + color.to_i + normal.to_i

    result = result + @pos if pos != nil
    result = result + @color if color != nil
    result = result + @normal if normal != nil

    raise("Error converting Vertex to array. Generated array: " + result.to_s + ". Expected length: " + expectedSize.to_s) if result.length != expectedSize

    result
  end

  def Vertex.setGLModes(pos = nil, color = nil, normal = nil)
    totalSize = pos.to_i + color.to_i + normal.to_i
    colorOffset = 0

    if pos != nil then
      glEnableClientState(GL_VERTEX_ARRAY)
      glVertexPointer(pos, GL_FLOAT, totalSize * 4, 0)
      colorOffset += pos
    else
      glDisableClientState(GL_VERTEX_ARRAY)
    end

    if color != nil then
      glEnableClientState(GL_COLOR_ARRAY)
      glColorPointer(color, GL_FLOAT, totalSize * 4, colorOffset * 4)
    else
      glDisableClientState(GL_COLOR_ARRAY)
    end
  end
end
