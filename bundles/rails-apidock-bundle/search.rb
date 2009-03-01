#! /usr/bin/env ruby

require 'rubygems'
require 'activesupport'

def create_data(data_file, file)
  result = []
  File.open(file) do |f|
    str = f.read
    keywords,data = str.split(";")
    data.gsub!(/^[^=]+= /, "")
    decoded = ActiveSupport::JSON.decode(data)
    for it in decoded
      i = {
        :score => it["score"],
        :name => it["name"],
        :path => it["path"] == "-" ? nil : it["path"],
        :type => it["type"],
        :method_type => it["method_type"]
      }
      result << i
    end
  end
  File.open(data_file, File::WRONLY | File::TRUNC | File::CREAT) do |f|
    f.write Marshal.dump(result)
  end
end

def get_data(file)
  data_file = "#{file}.data"
  unless File.exists?(data_file)
    create_data(data_file, file)
  end
  Marshal.load(File.open(data_file))
end

def search(mod, str)
  file = File.dirname(__FILE__) + "/#{mod}.js"
  data = get_data(file)
  q = Regexp.new(Regexp.quote(str), Regexp::IGNORECASE)
  result = []
  for it in data
    next unless q.match(it[:name])
    it[:score] = it[:score].to_f
    result << it
  end
  return if result.empty?

  result = result.sort_by{ |a| a[:score] }.reverse
  to_lisp(mod, result)
end

def to_lisp(mod, data)
  result = []
  for it in data
    url_prefix = if it[:path]
                   "#{it[:path].gsub(/\:\:/, "/")}/"
                 else
                   ""
                 end
    url_prefix = "http://apidock.com/#{mod}/#{url_prefix}"
    line = case it[:type]
           when "method"
             if it[:method_type] == "instance"
               ["#{it[:path]}##{it[:name]}", "#{url_prefix}#{it[:name]}"]
             else
               ["#{it[:path]}.#{it[:name]}", "#{url_prefix}#{it[:name]}/class"]
             end
           when "class", "module"
             title = it[:path] ? "#{it[:path]}::#{it[:name]}" : it[:name]
             [title, "#{url_prefix}#{it[:name]}"]
           end
    result << line
  end
  result.map! do |i|
    '("' + i.first + '" . "' + i.last + '")'
  end
  "(" + result.join("\n") + ")"
end

puts search("rails", ARGV.first.to_s.strip) if ARGV.first
