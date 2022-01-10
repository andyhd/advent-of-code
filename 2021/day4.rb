input = File.open("day4-input.txt")

numbers = input.readline().chomp().split(",").map(&:to_i)

input.readline()

class Board
  attr_reader :rows
  def initialize(rows)
    @rows = rows.split("\n").map { |row| row.chomp().split(" ").map(&:to_i) }
    @height = @rows.length
    @width = @rows[0].length
    @rows.flatten!
    @marked = @rows.dup
  end
  def complete_row()
    (0...@height).find { |row|
      row_start = row * @width
      row_end = (row + 1) * @width
      row_range = (row_start...row_end)
      @marked[row_range].none?
    }
  end
  def complete_col()
    (0...@width).find { |col_start|
      col_end = col_start + (@height - 1) * @width
      col_range = (col_start..col_end).step(@width)
      @marked[col_range].none?
    }
  end
  def mark(number)
    index = @rows.index(number)
    if not index.nil?
      @marked[index] = nil
    end
  end
  def win()
    complete_row or complete_col
  end
  def sum_unmarked()
    @marked.select { |num| not num.nil? }.sum
  end
end

boards = []
while true
  board_input = input.gets("")
  if board_input.nil?
    break
  end
  boards.append(Board.new(board_input.chomp()))
end

boards

winner = nil

while winner.nil?
  number = numbers.shift
  boards.map { |board| board.mark(number) }
  winner = boards.find_index { |board| board.win }
end

winner

boards[winner].sum_unmarked * number

winners = [winner]

while winners.length < boards.length - 1

  number = numbers.shift

  boards.map { |board| board.mark(number) }

  winners = boards.find_all { |board| board.win }

end

winners.length

boards.length

last = boards.to_set - winners.to_set

last_board = last.to_a[0]

last_board

last_board.win

number

last_board.mark(number)

last_board.sum_unmarked * number
