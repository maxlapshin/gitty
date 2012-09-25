#!/usr/bin/env ruby
class String
  if ((defined? RUBY_VERSION) && (RUBY_VERSION[0..2] == "1.9"))
    def getord(offset); self[offset].ord; end
  else
    alias :getord :[]
  end
end

require 'zlib'
require 'digest/sha1'


OBJ_TYPES = [nil, :commit, :tree, :blob, :tag].freeze

class LooseObjectError < StandardError
      end

def legacy_loose_object?(buf)
  word = (buf.getord(0) << 8) + buf.getord(1)
  puts "Z: #{buf.getord(0)}, #{buf.getord(1)}, #{word}, #{word % 31}"
  buf.getord(0) == 0x78 && word % 31 == 0
end


def unpack_object_header_gently(buf)
  used = 0
  c = buf.getord(used)
  used += 1

  type = (c >> 4) & 7;
  size = c & 15;
  shift = 4;
  while c & 0x80 != 0
    if buf.bytesize <= used
      raise LooseObjectError, "object file too short"
    end
    c = buf.getord(used)
    used += 1

    size += (c & 0x7f) << shift
    shift += 7
  end
  type = OBJ_TYPES[type]
  if ![:blob, :tree, :commit, :tag].include?(type)
    raise LooseObjectError, "invalid loose object type"
  end
  return [type, size, used]
end


def get_raw_object(buf)
  if buf.bytesize < 2
    raise LooseObjectError, "object file too small"
  end

  if legacy_loose_object?(buf)
    puts "Legacy"
    content = Zlib::Inflate.inflate(buf)
    header, content = content.split(/\0/, 2)
    if !header || !content
      raise LooseObjectError, "invalid object header"
    end
    type, size = header.split(/ /, 2)
    if !%w(blob tree commit tag).include?(type) || size !~ /^\d+$/
      raise LooseObjectError, "invalid object header"
    end
    type = type.to_sym
    size = size.to_i
  else
    type, size, used = unpack_object_header_gently(buf)
    content = Zlib::Inflate.inflate(buf[used..-1])
  end
  raise LooseObjectError, "size mismatch" if content.bytesize != size
  # return RawObject.new(type, content)
  [type, content]
end


path = ".git/objects/11/b37b1f3fa8db5891b1078ddc83d1faf071961e"
c = open(path, 'rb') { |f| f.read }
puts get_raw_object(c).inspect


