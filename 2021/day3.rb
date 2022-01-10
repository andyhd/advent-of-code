require 'matrix'


class BitArray
  attr_reader :bits
  def initialize(args)
    case args
    when String
      @bits = args.chomp.split("").map(&:to_i)
    when Vector
      @bits = args.to_a
    else
      @bits = args
    end
  end
  def to_i()
    to_s().to_i(2)
  end
  def to_s()
    @bits.join("")
  end
  def size()
    @bits.size
  end
end

def count_set(bit_arrays)
  bit_arrays.reduce(nil) do |counts, bit_array|
    counts = Vector.zero(bit_array.size) if counts.nil?
    counts + Vector.elements(bit_array.bits)
  end
end

class BitArraySet
  attr_reader :counts, :size, :items, :entry_size
  def initialize(bit_arrays)
    @items = bit_arrays
    @counts = count_set(bit_arrays)
    @size = bit_arrays.size
    @entry_size = bit_arrays.first.size
  end
  def to_s()
    @items.map(&:to_s).join(", ")
  end
  def most_common_value(index)
    half_size = @size / 2.0
    @counts[index] >= half_size ? 1 : 0
  end
  def least_common_value(index)
    (most_common_value(index) - 1).abs
  end
end

class Report
  def initialize(input)
    @entries = BitArraySet.new(input.map(&BitArray.method(:new)))
  end
  def length()
    @entries.size
  end
  def entries()
    @entries.items
  end
  def gamma()
    meth = @entries.method(:most_common_value)
    BitArray.new((0...@entries.entry_size).collect(&meth)).to_i
  end
  def epsilon()
    meth = @entries.method(:least_common_value)
    BitArray.new((0...@entries.entry_size).collect(&meth)).to_i
  end
  def power_consumption()
    gamma() * epsilon()
  end
  def oxygen_generator_rating()
    ((0...@entries.entry_size).reduce(@entries.dup) { |candidates, index|
      if candidates.size == 1
        candidates
      else
        BitArraySet.new(candidates.items.select { |bits|
          bits.bits[index] == candidates.most_common_value(index)
        })
      end
    }).items.first.to_i
  end
  def co2_scrubber_rating()
    ((0...@entries.entry_size).reduce(@entries.dup) { |candidates, index|
      if candidates.size == 1
        candidates
      else
        BitArraySet.new(candidates.items.select { |bits|
          bits.bits[index] == candidates.least_common_value(index)
        })
      end
    }).items.first.to_i
  end
  def life_support_rating()
    return oxygen_generator_rating() * co2_scrubber_rating()
  end
end

report = Report.new(File.foreach("day3-input.txt"))

p "gamma = #{report.gamma}, epsilon = #{report.epsilon}, power consumption = #{report.power_consumption}"

report.life_support_rating()
