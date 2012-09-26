#!/usr/bin/env ruby


$:.unshift("../grit/lib")

require 'grit'

repo = Grit::Repo.new("test/dot_git", {:is_bare => true})

# blob = repo.object("10c141ddad72cf3d1b4d453b3a3f404fc89618b5")
blob = repo.object("d8c6431e0a82b6b1bd4db339ee536f8bd4099c8f")
# puts blob.content

