input = File.foreach("day5-test-input.txt")

lines = input.map do |line|
  line.chomp().split(" -> ").map do |point|
    point.split(",").map(&:to_i)
  end
end

maxima = lines.reduce([0, 0]) do |maxima, line|
  case line
  in [[start_x, start_y], [end_x, end_y]]
  end
  [
    [maxima[0], start_x, end_x].max,
    [maxima[1], start_y, end_y].max
  ]
end

grid = Array.new(maxima.reduce(:*), 0)

lines.
