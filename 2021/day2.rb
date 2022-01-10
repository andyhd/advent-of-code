course = File.foreach("day2-test-input.txt")


class Submarine
  def initialize
    @pos = 0
    @depth = 0
    @aim = 0
  end
  def forward value
    @pos += value
    @depth += value * @aim
  end
  def down value
    @aim += value
  end
  def up value
    @aim -= value
  end
  def pos
    @pos
  end
  def depth
    @depth
  end
  def aim
    @aim
  end
end


def parse(input)
  Enumerator.new do |y|
    File.foreach(input) do |line|
      command, value = line.split(" ")
      y.yield command, value.to_i
    end
  end
end

sub = Submarine.new

parse("day2-input.txt").map do |command, value|
  sub.send(command, value)
end

p "pos = #{sub.pos}, depth = #{sub.depth}"
p "result = #{sub.pos * sub.depth}"
