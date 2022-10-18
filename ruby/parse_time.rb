source_tz = nil
until ARGV.empty? do
  source_tz = ARGV.shift
end
dateString = gets

_init_time
date = if dateString.blank? then Time.now else parse_time dateString end
require 'tzinfo'
# https://apidock.com/ruby/Time/strftime
if !source_tz.nil?
  tz = TZInfo::Timezone.get source_tz
  date = Time.parse(date.strftime("%FT%T.%L") + tz.to_local(Time.now.utc).strftime("%:z"))
end
utc_date = date.utc
#tz = TZInfo::Timezone.get(target_tz)
format = l {|t|
  t.strftime "%a %b %d %Y %I:%M %p %Z"
}
puts "#{utc_date.strftime "%FT%T.%L%:z"}"
puts "#{(utc_date.to_f * 1000).to_i}"
puts "#{utc_date.strftime "%s"}"
#puts "#{date}"
#puts "#{tz.to_local date}"
df = TZInfo::Timezone.get("America/Los_Angeles").to_local utc_date
puts "#{format[df]}"
df = TZInfo::Timezone.get("America/Chicago").to_local utc_date
puts "#{format[df]}"
df = TZInfo::Timezone.get("America/New_York").to_local utc_date
puts "#{format[df]}"
df = TZInfo::Timezone.get("Etc/UTC").to_local utc_date
puts "#{format[df]}"
#x = "UT - #{format[df]}"
#puts "#{x[0..-4]}:00#{x[-3..-1]}"
