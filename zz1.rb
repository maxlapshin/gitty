#!/usr/bin/env ruby


$:.unshift("../grit/lib")

require 'grit'

repo = Grit::Repo.new(".git")

t1 = Time.now

1000.times do
  data = repo.head.commit.tree./("src/gitty.app.src").data
  raise Error if data.size != 228
end

t2 = Time.now

puts t2-t1
