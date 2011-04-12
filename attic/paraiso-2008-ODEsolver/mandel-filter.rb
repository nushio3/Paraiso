#!/usr/bin/env ruby

def d(x,y)
  x*x + y*y
end

while line = gets
  nums = line.split(/\s/)
  next if nums[2]=~ /[Nn][Aa][Nn]/
  next if d(nums[2].to_f , nums[3].to_f) > 100
  puts "#{nums[0]} #{nums[1]}"
end
