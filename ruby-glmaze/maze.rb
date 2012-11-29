require 'opengl'
require 'vertex'

class Maze
  WallHeight = 1
  
  def initialize(grid)
    @grid = grid
  end

  def genGeometry
    v = []

    height = @grid.length
    width = @grid[0].length

    @numTris = 0

    # Iterate through the grid horizontally.
    # Generating the walls at the top and bottom of the cells
    (1..height - 2).each do |row|
      startTop = nil
      startBottom = nil
      (1..width - 1).each do |col|
        thisCell = @grid[row][col]
        upCell = @grid[row - 1][col]
        downCell = @grid[row + 1][col]

        startTop = col if upCell == true && thisCell == false && startTop == nil
        startBottom = col if downCell == true && thisCell == false && startBottom == nil

        if (startTop != nil && (thisCell == true || upCell != true)) then
          v.push(Vertex.new(pos = [col, WallHeight, row], color = [1, 0, 0]))
          v.push(Vertex.new(pos = [startTop, 0, row], color = [1,0,0]))
          v.push(Vertex.new(pos = [startTop, WallHeight, row], color = [1, 0, 0]))

          v.push(Vertex.new(pos = [col, WallHeight, row], color = [0, 1, 0]))
          v.push(Vertex.new(pos = [col, 0, row], color = [0, 1, 0]))
          v.push(Vertex.new(pos = [startTop, 0, row], color = [0, 1, 0]))
      
          @numTris += 2
          startTop = nil
        end

        if (startBottom != nil && (thisCell == true || downCell != true)) then
          v.push(Vertex.new(pos = [col, WallHeight, row + 1], color = [1, 0, 0]))
          v.push(Vertex.new(pos = [startBottom, 0, row + 1], color = [1,0,0]))
          v.push(Vertex.new(pos = [startBottom, WallHeight, row + 1], color = [1, 0, 0]))

          v.push(Vertex.new(pos = [col, WallHeight, row + 1], color = [0, 1, 0]))
          v.push(Vertex.new(pos = [col, 0, row + 1], color = [0, 1, 0]))
          v.push(Vertex.new(pos = [startBottom, 0, row + 1], color = [0, 1, 0]))
      
          @numTris += 2
          startBottom = nil
        end
      end
    end

    # Iterate through the grid vertically.
    # Generating the walls at the left and right of the cells
    (1..width - 2).each do |col|
      startLeft = nil
      startRight = nil
      (1..height - 1).each do |row|
        thisCell = @grid[row][col]
        leftCell = @grid[row ][col - 1]
        rightCell = @grid[row][col + 1]

        startLeft = row if leftCell == true && thisCell == false && startLeft == nil
        startRight = row if rightCell == true && thisCell == false && startRight == nil

        if (startLeft != nil && (thisCell == true || leftCell != true)) then
          v.push(Vertex.new(pos = [col, WallHeight, startLeft], color = [1, 0, 0]))
          v.push(Vertex.new(pos = [col, 0, startLeft], color = [1,0,0]))
          v.push(Vertex.new(pos = [col, WallHeight, row], color = [1, 0, 0]))

          v.push(Vertex.new(pos = [col, WallHeight, row], color = [0, 1, 0]))
          v.push(Vertex.new(pos = [col, 0, startLeft], color = [0, 1, 0]))
          v.push(Vertex.new(pos = [col, 0, row], color = [0, 1, 0]))
      
          @numTris += 2
          startLeft = nil
        end

        if (startRight != nil && (thisCell == true || rightCell != true)) then
          v.push(Vertex.new(pos = [col + 1, WallHeight, startRight], color = [1, 0, 0]))
          v.push(Vertex.new(pos = [col + 1, 0, startRight], color = [1,0,0]))
          v.push(Vertex.new(pos = [col + 1, WallHeight, row], color = [1, 0, 0]))

          v.push(Vertex.new(pos = [col + 1, WallHeight, row], color = [0, 1, 0]))
          v.push(Vertex.new(pos = [col + 1, 0, startRight], color = [0, 1, 0]))
          v.push(Vertex.new(pos = [col + 1, 0, row], color = [0, 1, 0]))
      
          @numTris += 2
          startRight = nil
        end
      end
    end

    puts v.to_s
    puts @numTris.to_s

    data = v.map {|x| x.toArray(pos = 3, color = 3)}
    data.flatten!
    data = data.pack("f*")

    @vbuffer = glGenBuffers(1)
    glBindBuffer(GL_ARRAY_BUFFER, @vbuffer[0])
    glBufferData(GL_ARRAY_BUFFER, 4 * data.length, data, GL_STATIC_DRAW)
  end

  def freeGeometry
    glDeleteBuffers(@vbuffer)
  end

  def render
    Vertex.setGLModes(pos = 3, color = 3)
    glBindBuffer(GL_ARRAY_BUFFER, @vbuffer[0])
    glDrawArrays(GL_TRIANGLES,0, @numTris * 3)
  end
end
