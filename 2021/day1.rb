readings = File.foreach("day1-input.txt").map { |depth| depth.to_i }


module Enumerable
  def each_with_previous(&block)
    if block
      self.inject(nil) { |prev, curr| yield prev, curr; curr }
      self
    else
      Enumerator.new do |y|
        self.inject(nil) { |prev, curr| y.yield(prev, curr); curr }
      end
    end
  end
end


def increasing(prev, curr)
  not prev.nil? and curr > prev
end


def count_increasing(enum)
  enum.each_with_previous.count { |prev, curr| increasing(prev, curr) }
end


count_increasing(readings)


count_increasing(readings.each_cons(3).map{|w| w.sum})
