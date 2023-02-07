require 'shellwords' # for Shellwords.escape
require 'stringio' # for capture_stdout
require 'ostruct' # for Hash.os

alias l lambda
alias $F $FILENAME


$bash_shopt_cmd="cd;shopt -s expand_aliases;. .bash_profile;cd - > /dev/null;\n"
def bash(string)
	system("bash","-c",$bash_shopt_cmd+string)
#	system("bash","-i -c",string)
end
def make_temp
	`mktemp`.chomp
end
def write_temp(string)
	out = make_temp
	out.write string
	out
end
def read_temp(fn)
	txt = fn.read
	`rm #{fn}`
	txt
end
$bash_get_exit_code="; echo $? > "
def bspawn(string)
	out = make_temp
	outES = out+"ES"
	# TODO: switch script to PTY https://ruby-doc.org/stdlib-2.5.3/libdoc/pty/rdoc/PTY.htm
	{ pid:spawn("bash","-c","script -q -c #{Shellwords.escape $bash_shopt_cmd + string + $bash_get_exit_code + outES} /dev/null", :err=>:out, :out=>out),
		out:out, outES:outES }.os
end
def getresults(cmds)
	results = {}
	stty=`stty -g`
	cmds.each{|k,v|
		cmd = v.nil? ? k : v
		result = bspawn(cmd)
		key = v.nil? ? result.pid : k
		results[key] = result
		`stty #{stty}`
		puts "task: #{key} output:"
		puts "tail -f #{result.out}"
	}
	results.each{|k,v|
		Process.wait v.pid
		v.exitstatus = v.outES.read.to_i
	}
	`stty #{stty}`
	return results
end
def getresults!(cmds)
	results = getresults cmds
	haserr=false
	results.each{|k,v|
		#if v.exitstatus == 0
		#	`rm #{v.out}`
		#	`rm #{v.outES}`
		#end
		if v.exitstatus != 0
			puts k.to_s + " failed with exit code: " + v.exitstatus.to_s
			haserr=true
		end
	}
	raise "some tasks failed" if haserr
	return results
end
def processresults(results, print=false, throwonerr=false)
	haserr=false
	results.each{|k,v|
		if print
			v.out.read.mylines.each{|l|
				puts k.to_s + ": " + l
			}
		end
		if !throwonerr || v.exitstatus == 0
			`rm #{v.out}`
			`rm #{v.outES}`
		end
		if throwonerr && v.exitstatus != 0
			puts k.to_s + " failed with exit code: " + v.exitstatus.to_s
			haserr=true
		end
	}
	raise "some tasks failed" if haserr
end
def printresults(results)
	processresults results, true
end
def runtasks(cmds)
	printresults getresults cmds
end
def runtasks!(cmds)
	processresults (getresults cmds), false, true
end


def include_case_transform
	if !$_case_transforml_called
        require "active_support/isolated_execution_state"
		require 'case_transform'
		$_case_transform_called = true
	end
end

$_rnd_escape_sprintf = '6zriRijNQ5vdnYjxVb9yvr0742Okj4U0ZM0zK8qQ'
class String
    def camel
        include_case_transform
        CaseTransform.camel_lower self
    end
    def pascal
        include_case_transform
        CaseTransform.camel self
    end
    def snake
        include_case_transform
        CaseTransform.underscore self
    end
    def underscore
        include_case_transform
        CaseTransform.underscore self
    end
    def kebab
        include_case_transform
        CaseTransform.dash self
    end
  def blank?
    return self.nil? || self.empty? || (self =~ /\A\s*\Z/ ? true : false)
  end
	def mylines(&block)
		if block != nil
			self.lines.each_with_index{|l,i|
				case block.arity
					when 1
						yield l.chomp
					when 2
						yield l.chomp, i
					else
						yield l.chomp
				end
			}
		else
			self.lines.map{|l| l.chomp}
		end
	end
	def arrays(regex, &block)
		if block != nil
			self.mylines{|l,i|
				la = l.split(regex,-1)
				case block.arity
					when 1
						yield la
					when 2
						yield la, i
					else
						yield la
				end
			}
		else
			self.mylines.map{|l| l.split(regex,-1)}
		end
	end
	def arrayst(&)
		 self.arrays(/\t/, &)
	end
	def arraysc(&)
		 self.arrays(/,/, &)
	end
  # split string into lines then split each line with provided regex and return openstruct objects with provided field names
	def os(regex, *colnames)
		colnames=colnames[0] if colnames.size==1 && colnames[0].kind_of?(Array)
		self.arrays(regex).map{|a|
			o={}
			colnames.zip(a).each{|n,e|
				if !e.nil? && !n.nil?
					o["#{n}"] = e
				end
			}
			o.os
		}
	end
	def ost *colnames
		self.os(/\t/, *colnames)
	end
	def osc *colnames
		self.os(/,/, *colnames)
	end
	def osj
		self.json(object_class: OpenStruct)
	end
	def read
		if self == "-"
			$<.read
		else
			IO.read(self)
		end
	end
	def binread
		IO.binread(self)
	end
	def readb
		IO.read(self).b # same as binread, but a useful reminder about the "b" method on String
		# Method: String#b - Returns a copied string whose encoding is ASCII-8BIT. same as .force_encoding("ASCII-8BIT")
	end
	def read_mssql
		IO.read(self, {encoding:'utf-16le',mode:'rb'}).encode('utf-8').gsub("\r",'')
	end
	def read_temp
		super self
	end
	def write_temp
		super self
	end
	def writeto(fileName)
		IO.write(fileName, self)
	end
		alias_method :write_to, :writeto
	def write(text)
		IO.write(self, text)
	end
	def exists?
		File.exists? self
	end
	def load
		data = nil
		File.open(self,"rb"){|f|
			data = Marshal.load f
		}
		return data
	end
	alias_method :old_dump, :dump
	def dump *args, &block
		if args.length == 1
			File.open(self,"wb"){|f|
				Marshal.dump args[0], f
			}
			return self
		end
		old_dump *args, &block
	end
	def subfile old, new
		self.write self.read.sub old, new
	end
    def rx
        Regexp.new(Regexp.quote(self))
    end
    def rxq
        Regexp.quote(self)
    end
    def rxt rx_factory: Regexp.method(:new)
        self.split(/%(?<brace>{[^{}]*(?:\g<brace>[^{}]*)*})/).mapi{|x,i|
			if i.even?
				x.rx
			else
				rx_factory.call x[1..-2]
			end
		}.reduce //, :+
    end
    def shellq
    	Shellwords.escape(self)
    end
    def shq
    	Shellwords.escape(self)
    end
	def env
		ENV[self]
	end
    def txt
        "#{Dir.home}/tmp/clip_file_#{self}".read
    end
	def bash
		`bash -c #{Shellwords.escape($bash_shopt_cmd+self)}`
#		`bash -i -c #{Shellwords.escape(self)}`
	end
	def bash!
		rval = bash
		raise "bash! exited with error for command:\n#{self}" if $?.exitstatus != 0
		rval
	end
	def exec
		Kernel.exec "bash -c #{Shellwords.escape($bash_shopt_cmd+self)}"
	end
    
    alias_method :old_upcase, :upcase
    def upcase *args
      if args.length==0
        return old_upcase
      end
      rval = self.dup
      rval[args[0]] = self[args[0]].upcase
      rval
    end

    alias_method :old_downcase, :downcase
    def downcase *args
      if args.length==0
        return old_downcase
      end
      rval = self.dup
      rval[args[0]] = self[args[0]].downcase
      rval
    end
    
    alias_method :old_interpolate, '%'.to_sym
    def % *args, &block
      self_clean_escapes=self
	  .gsub(/%{%([^}]+)}/, $_rnd_escape_sprintf + '\1') # this allows the normal Kernel.sprintf formatting e.g. puts "%{%02X}" % 5
	  .gsub(/%(?!{)/,'%%')                            # automatically escape any instance of % in the string, except if followed by {
	  .gsub('%{"', '%%{')                             # this allows escaping %{
	  .gsub('%{}','%s')
	  .gsub($_rnd_escape_sprintf,'%')
	        
      if args.length == 1
        if args[0].is_a?(OpenStruct)
          return self_clean_escapes.old_interpolate args[0].to_h
        elsif args[0].is_a?(Hash)
          return self_clean_escapes.old_interpolate args[0]
        elsif args[0].is_a?(Array)
          return self.old_interpolate args[0]         # if passing in array, then we can't have named, %{}, syntax
        else
          return self.old_interpolate args[0]
        end
      end
      
      # find start and end anchors in this string and replace with replacement(s)
      # if more than one replacement is provided then each anchor match gets a different replacement
      # leading whitespace of end anchor is preserved
      # have to use .% syntax to call it
      # e.g. txt.% "start\n", "end", ["a", "b", "c"]
      if args.length == 3
      	startAnchor, endAnchor, replacements = args
      	e = (replacements.respond_to?(:each) ? replacements : [replacements]).to_enum
      	return self.gsub(startAnchor.rx + /(?m).*?(\n?\s*)/ + endAnchor.rx) {
      	  "#{startAnchor}#{e.next}#$1#{endAnchor}"
      	}
      end
      
      if args.length == 0 && !block.nil?
      	h = {}
      	b = block.binding
      	b.local_variables.each{|x|
      		h[x] = eval x.to_s, b
      		if h[x].is_a?(OpenStruct) || h[x].is_a?(Hash)
				begin
					os = h[x].os
					os.instance_variable_get("@table").keys.each{|okey|
						h[x+'.'+okey]=os[okey]
					}
				rescue
				end
      		end
      	}
      	return self_clean_escapes.old_interpolate h
      end
	  
	  raise "Got to unknown state in string interpolate function. self #{self.inspect} args #{args.inspect} block #{block.inspect}"
    end
    
end

def include_method_source
	if !$_method_source_called
		require 'method_source'
		require 'digest/sha1'
		$_method_source_called = true
	end
end

# usage:
# sql_rows = ini do
#    "select * from table".sql.to_a
# end
#
# optional version parameter:
# -1 - don't cache
# -2 - don't run, return nil
# <integer> - specific version number
def memo version = nil, saveOnExit = false, &block
    include_method_source
	variable, block_source = block.source.split /\ ?= /
	return yield if version == -1
	return nil if version == -2
	if version.nil?
		include_method_source
		version = Digest::SHA1.hexdigest(block.source)
	end
	dataFile = "#{Dir.pwd}/.ruby_data_#{variable}#{version.nil? ? "" : "_#{version}"}"
	block.binding.eval("$__#{variable}__dataFile='#{dataFile}'")
	block.binding.eval(%Q[END { "#{dataFile}".dump #{variable} }]) if saveOnExit
	if File.exists?(dataFile) && (version.is_a?(Integer) && version > -1 || !version.is_a?(Integer))
		dataFile.load
	else
		ruby_marshal_data = yield
		dataFile.dump ruby_marshal_data
		ruby_marshal_data
	end
end

def memo! version = nil, &block
	memo version, true, &block
end

class Symbol
    def env
        ENV[self.to_s]
    end
    def txt
        "#{Dir.home}/tmp/clip_file_#{self}".read
    end
    def + to
    	return "#{self}#{to}".to_sym        
    end
end
def include_open3
	if !$_open3_called
		require 'open3'
		$_open3_called = true
	end
end

# usage:
# [1,2,3].then 'Set.new'  <- directly evals any strings, so you don't have to mess with finding methods
# [1,2,3].map 'puts'      <- directly evals any strings, so you don't have to mess with finding methods
# [1,2,3].map :+, 5       <- sends all arguments
# $x=6
# 5.then{|x| x+1}.then{|x| x+1}.then{|x| x+1}.then '$x='
# p $x
# TODO: regex magic variables will not be properly set if we don't evaluate map in the block's binding
def _my_map old_map, *args, &block
    if args.size > 0 and block.nil?
        if args[0].is_a? String
            old_map.call{|x|eval "#{args[0]} x #{args[1..].map{|y|", #{y}"}.join}"}
        elsif args[0].is_a? Symbol
            old_map.call{|x|x.__send__ *args}
        else
            raise "_my_map expects a string or symbol to invoke, but got a #{args[0].class}"
        end
    else
        old_map.call *args, &block
    end
end

module Enumerable
    alias_method :old_map, :map
	# this calls method specified in arg[0] on each element of the array
	def map *args, &block
	    _my_map self.method(:old_map), *args, &block
	end
	def mapi(&)
		self.each_with_index.map(&)
	end
	def eachi(&)
		self.each_with_index(&)
	end
	def eacho *args, &block
		if args.size == 0
			self.each_with_object *[{}.os], &block
		else
			self.each_with_object *args, &block
		end
	end
	def eachh(&)
		self.each_with_object({}, &)
	end
	def eachs(&)
		self.each_with_object(Set.new, &)
	end
	def eacha(&)
		self.each_with_object([], &)
	end
	def runall
		include_open3
		threads = self.mapi{|(k,v),i|
			cmd = v.nil? ? k : v
			key = v.nil? ? nil : k
			Thread.new do
				if cmd.is_a? Proc
					read, write = IO.pipe
					fork { Marshal.dump cmd.call, write }
					Process.wait
					write.close
					Thread.current[:rval] = [(Marshal.load read),nil,$?]
					Thread.current[:key] = key
					read.close
				else
					# stdout, stderr, status
					Thread.current[:rval] = Open3.capture3("bash", "-c", $bash_shopt_cmd+cmd)
					Thread.current[:key] = key
				end
			end
		}
		joinedThreads = threads.map(&:join)
		if self.is_a?(Hash)
			joinedThreads.kmap(:key).zip(joinedThreads.kmap(:rval)).to_h
		else
			joinedThreads.kmap(:rval)
		end
		#runtasks self
	end
	def runall!
		runtasks! self
	end
	def to_set
		Set.new self
	end
	def kmap key
		self.map{|x|x[key]}
	end
end
class Array
	# don't know if need this version. takes an even-number list and makes a map
	# regular to_h takes a list of tuples instead
	def to_hh
		self.each_slice(2).to_h
	end
	alias_method :old_map, :map
	# this calls method specified in arg[0] on each element of the array
	def map *args, &block
	    _my_map self.method(:old_map), *args, &block
	end
	alias_method :old_each, :each
	def each *args, &block
		_my_map self.method(:old_each), *args, &block
	end

	alias_method :contains?, :include?
	
  def joinn
    join "\n"
  end
  def joinnn
    join "\n\n"
  end
  def joinnnn
    join "\n\n\n"
  end
  def joins
    join " "
  end
  def joinc
    join ","
  end
  def joincn
    join ",\n"
  end
  def joint
    join "\t"
  end
  
end

class Set
	alias_method :contains?, :include?
	alias_method :addAll, :merge
	alias_method :add_all, :merge
end

class Hash
	def self.of *args, &block
		if block.nil?
			Hash.new{|hash, key|
				hash[key] = args[0].call
			}
		else
			Hash.new{|hash, key|
				case block.arity
					when 0
						hash[key] = yield
					else
						hash[key] = yield key
				end
			}
		end
	end
	def of *args, &block
		Hash.of *args, &block
	end
	def self.of_sets
		Hash.of{{}.to_set}
	end
	def self.of_lists
		Hash.of{[]}
	end
	def self.of_maps
		Hash.of{{}}
	end
	def of_sets
		Hash.of{{}.to_set}
	end
	def of_lists
		Hash.of{[]}
	end
	def of_maps
		Hash.of{{}}
	end
    
	def eachi
		self.each.with_index{|e, i|
			yield e[0], e[1], i
		}
	end
	def os
        	OpenStruct.new(self)
	end
	def add key, value=nil
		self[key]=value
	end
	def to_set
		Set.new self.keys
	end
	def method_missing(meth, *args, &block)
		if block.nil?
			meth_name = meth.to_s
			if meth_name[-1] == '='
				self[meth_name[..-2].to_sym] = args[0]
			else
				if self.has_key?(meth)
					self[meth]
				elsif self.has_key?(meth_name)
					self[meth_name]
				elsif !self.default_proc.nil?
					self[meth]
				elsif meth_name[-1] == '?'
					meth_symb = meth_name[0..-2].to_sym
					if !self.has_key?(meth_symb)
						self[meth_symb] = args[0]
					end
					self[meth_symb]
				else
					super # When you invoke super with no arguments Ruby sends a message to the parent of the current object,
								# asking it to invoke a method of the same name as the method invoking super. It automatically forwards the
								# arguments that were passed to the method from which it's called.
								# here we call method_missing of object
				end
			end
		else
			super
		end
	end
	
	alias_method :contains_key?, :has_key?
	alias_method :containsKey?, :has_key?
	alias_method :contains?, :has_key?
	alias_method :hasKey?, :has_key?
	alias_method :get, :fetch
end
class File
	def self.dir?(path)
		return File.directory?(path)
	end
end
class Dir
	def self.cd(path)
		return Dir.chdir path
	end
end
class IO
	def self.write(path,text)
		File.open(path,'w'){|f|
			f.write(text)
		}
	end
end
class Regexp
	def +(r)
		Regexp.new(source + r.source)
	end
end
class Object
	alias_method :old_then, :then
	def then *args, &block
	    _my_map self.method(:old_then), *args, &block
	end
end
class OpenStruct
	# change hashcode and equals behavior
    def set_hash_keys! *args
    	define_singleton_method(:hash){
    		args.map{|keyName| self[keyName]}.to_a.hash
    	}
    	define_singleton_method(:eql?){|other|
    		args.all?{|keyName| self[keyName].eql? other[keyName]}
    	}
    	self
    end
    def os
        self
    end
	def keys
		self.instance_variable_get("@table").keys
	end
	alias_method :__keys, :keys
	def keys= val
		self[:keys] = val
		define_singleton_method(:keys){
    		self[:keys]
    	}
	end
end
 
module Kernel
 
  # usage:
  # out = capture_stdout{
  # 	<command>
  # }
  def capture_stdout
    out = StringIO.new
    $stdout = out
    yield
    return out.string
  ensure
    $stdout = STDOUT
  end
  
end

def source_code_lines
    if !$_source_code_lines_called
			$_source_code_lines = []
			if ($0 == "-e")
				$_source_code_lines << "/proc/#$$/cmdline".read
			else
				File.open($0, "r") {|f|
					while(line = f.gets)
						line.chomp!
						break if line == "__END__"
						$_source_code_lines << line
					end
				}
			end
    end
    $_source_code_lines
end

# def self.method_missing(meth, *args, &block)
# 	# TODO: automagically make missing values required command line args - How to know all the opts on just one method_missing? put field options behind #
# 	# re-use remaining parse options for further invocations - don't have to care about methods on object ??? OR just use opts.XXX
# 	# don't have full help text unless
# 	#
# 	# use
# #https://github.com/leejarvis/slop
# #https://github.com/ManageIQ/optimist/wiki
# #https://github.com/ManageIQ/optimist/wiki/Commands
# #https://github.com/ManageIQ/optimist/wiki/Option-Types
# #https://github.com/ManageIQ/optimist
# #https://www.manageiq.org/optimist/
# #https://github.com/ManageIQ/optimist/blob/master/lib/optimist.rb
# 	# see which library can calculate smallest common prefix for arguments
# 	# see init
#
#     #pp source_code_lines[caller_locations[0].lineno - 1]
# 	super
# end
#
# TODO: add commands - https://github.com/ManageIQ/optimist/wiki/Commands
# TODO: support shorter versions of long:, short:, default:
# examples:
# puts opts.car_color # sdfsdf,i
# puts opts.car_color_really # sdfsdf
# puts opts.boat_color? #what is,, the boat color???
# puts opts.have_car?
# #puts opts.have_car?#,req
# puts opts.have_car?
# opts.have_car = 999
# puts opts.have_car?
# pp opts.days#,type: :int,r,multi: true
# pp opts.names#,a
#
# FLAG_TYPES =
# The set of values that indicate a flag option when passed as the :type parameter of #opt.
## [:flag, :bool, :boolean]
# SINGLE_ARG_TYPES =
# The set of values that indicate a single-parameter (normal) option when passed as the :type parameter of #opt.
## A value of io corresponds to a readable IO resource, including a filename, URI, or the strings 'stdin' or '-'.
## [:int, :integer, :string, :double, :float, :io, :date]
# MULTI_ARG_TYPES =
# The set of values that indicate a multiple-parameter option (i.e., that takes multiple space-separated values on the commandline) when passed as the :type parameter of #opt.
## [:ints, :integers, :strings, :doubles, :floats, :ios, :dates]
#
# More usage: https://github.com/ManageIQ/optimist/blob/master/lib/optimist.rb#L113
#
# short options:
class MyOpts
	@@shortcuts = {
		r: 'required:true',
		m: 'multi:true',
		i: 'type: :int',
		f: 'type: :float',
		#a: array - see below
	}
	@@shortcut_keys = (@@shortcuts.keys + [:a]).to_set
	def initialize
		specs = {}.of_maps
		@opts = Optimist::options do
			educate_on_error

			optsrx = %r!(?:^|\W)opts\.(?<usage>\w+\??)(?<assignment>\s*=\s*)?(?<params>/(?:[^/]|//)*/)?(?<null_coalesce>\._\?)?!
			source_code_lines.each{|sourceCodeLine|
				sourceCodeLine.scan(optsrx){
					match = $~
					next if match['assignment']
					usage = match['usage']
					flag = usage[-1]=="?"
					name = flag ? usage[..-2] : usage
					spec = specs[name]
					spec.params = spec.params? []
					spec.flag? flag
					if match['null_coalesce']
						spec.params << "required:false"
					end
					params = match['params']
					if params
						params = params[1..-2].gsub ',,', $_rnd_escape_sprintf
						params = params.split ','
						is_array_params, params = params.partition{_1 == 'a'}
						spec.array = is_array_params.empty?.!
						params.eachi do |param, i|
							if param.starts_with? "="
								spec.params << "required:false"
								spec.params << "default: #{param[1..]}"
							elsif param.length == 1
								spec.params << @@shortcuts.get(param.to_sym, param)
							else
								if i == 0
									spec.desc? param.gsub($_rnd_escape_sprintf, ',')
								else
									spec.params << param
								end
							end
						end
					end
				}
			}

			for name, spec in specs
				params = spec.params? []
				optional = params.any?{_1[/\s*required:\s*false/]}
				params << 'required:true' unless optional
				required = params.any?{_1[/\s*required:\s*true/]}
				multi = params.any?{_1[/\s*multi:\s*true/]}
				type = nil
				params = params.map{
					if _1 =~ /\s*type:\s*(?<type>:\w+)/
						type = $~['type']
						if _1.to_s.ends_with? "s" or !spec.array? then _1 else _1 + "s" end
					else
						_1
					end
				}
				if type.nil?
					type = spec.flag ? ":bool" : ":string"
					params << "type: #{type}#{spec.array? ? "s" : ""}"
				end
				eval "opt :#{name}, '#{spec.desc? ''}#{multi ? " (multi)" : ""}#{required ? " (required)" : ""}', #{params.reject(&:empty?).joinc}"
			end
		end
	end

	def method_missing(meth, *args, &block)
		meth_name = meth.to_s
		if meth_name[-1,1] == '='
			@opts[meth_name[0..-2].to_sym] = args[0]
		else
			meth_name = meth_name.sub /\?$/, ''
			meth_name = meth_name.to_sym
			@opts[meth_name] = @opts[meth_name]._? args[0]
		end
	end

end

def opts
	if !$_opts_called
		require 'optimist'
		$opts = MyOpts.new
		$_opts_called = true
	end
	$opts
end


def _check_xml_called
	if !$_xml_called
		require 'rexml/document'
		#include REXML # so that we don't have to prefix everything with REXML::...
		REXML::Element.send(:define_method, 'each') do |*args, &block|
			each_element *args, &(block.nil? ? Proc.new{} : block)
		end
		REXML::Document.send(:alias_method, :old_write, :write)
		REXML::Document.send(:define_method, 'write') do |fn|
			old_write File.open(fn, 'w'), 2
		end
		REXML::Document.send(:alias_method, :to_s_raw, :to_s)
		#write(output=$stdout, indent=-1, transtive=false, ie_hack=false, encoding=nil)
		#write(options={:output => $stdout, :indent => -1, :transtive => false, :ie_hack => false, :encoding → nil})
		REXML::Document.send(:define_method, 'to_s') do |obj=nil, params=2|
			if obj.nil?
				s = ""
				old_write s, params
				s
			else
				old_write obj, params
			end
		end
		$_xml_called = true
	end
end
class String
	def xml
		_check_xml_called
		REXML::Document.new(self)
	end
end

class String
	def csv headers=false, **kwargs
		if !$_csv_called
			require 'csv'
			$_csv_called = true
		end
		kwargs[:headers] = headers
		CSV.parse self, **kwargs
	end
	def csv_os_helper table
		colnames = []
		usedNames = {}.to_set
		getNewNameL = l {|name, colIndex|
			if colnames[colIndex]
				next colnames[colIndex]
			end
			newName = name._?('col' + (colIndex + 1).to_xlscol).camel
			i = 1
			while usedNames.contains? [newName, i]
				i += 1
			end
			usedNames << [newName, i]
			newName = newName + (i == 1 ? '' : "_#{i}")
			colnames[colIndex] = newName
			newName
		}
		table.map{|row| o={};table.headers.eachi{|header, i| o[getNewNameL[header, i]] = row[i]};o.os}
	end
	def csv_os
		csv_os_helper self.csv(true)
	end
	def csvt headers=false
		csv headers, col_sep:"\t"
	end
	def csvt_os
		csv_os_helper self.csvt(true)
	end
	def csv!
		self.mylines.map{|x|x.split ','}
	end
end
def include_json
	if !$_json_called
		require 'json'
		$_json_called = true
	end
end
class String
	def json opts = nil
		include_json
		JSON.parse(self, opts)
	end
end
def include_yaml
	if !$_yaml_called
		require 'yaml'
		$_yaml_called = true
	end
end
class VanillaString < String
	undef_method :read
end
class String
	def yaml
		include_yaml
		YAML.load(VanillaString.new(self))
	end
end
class Array
	def to_j opts = nil
		include_json
        #indent: a string used to indent levels (default: ''),
        #space: a string that is put after, a : or , delimiter (default: ''),
        #space_before: a string that is put before a : pair delimiter (defau: ''),
        #object_nl: a string that is put at the end of a JSON object (default: ''),
        #array_nl: a string that is put at the end of a JSON array (default: '')
        #allow_nan: true if NaN, Infinity, and -Infinity should be generated, oerwise an exception is thrown if these values are encountered. This options defaults to false.
        #max_nesting: The maximum depth of nesting allowed in the data structures from which JSON is to be generated. Disable depth checking with :max_nesting => false, it defaults to 100.
        #See also the #fast_generate for the fastest creation method with the least amount of sanity checks, and the #pretty_generate method for some defaults for pretty output.
		JSON.pretty_generate(self, opts)
	end
	def to_j! opts = nil
		include_json
		JSON.generate(self, opts)
	end
end
class Hash
	def to_j opts = nil
		include_json
		JSON.pretty_generate(self, opts)
	end
	def to_j! opts = nil
		include_json
		JSON.generate(self, opts)
	end
end
class OpenStruct
	def to_j opts = nil
		include_json
		JSON.pretty_generate(self.to_h, opts)
	end
	def to_j! opts = nil
		include_json
		JSON.generate(self.to_h, opts)
	end
end
class String
	def to_j opts = nil
		include_json
		JSON.pretty_generate(self, opts)
	end
	def to_j! opts = nil
		include_json
		JSON.generate(self, opts)
	end
end
def include_inflections
	if !$_inflections_called
		require 'active_support/core_ext/string/inflections'
		$_inflections_called = true
		ActiveSupport::Inflector.inflections do |inflect|
			inflect.irregular 'tooth', 'teeth'
			inflect.irregular 'foot', 'feet'
			inflect.irregular 'syllabus', 'syallabi'
			inflect.irregular 'colossus', 'colossi'
			inflect.irregular 'goose', 'geese'
			inflect.irregular 'die', 'dice'
			inflect.uncountable %w(deer shrimp bison moose)
		end
	end
end
class String
	def pluralize count=2
		include_inflections
		count > 1 ? self.pluralize : self
	end
	def singularize count=1
		include_inflections
		count > 1 ? self : self.singularize
	end
end
module Enumerable
	def matrix
		if !$_matrix_called
			require 'matrix'
			Matrix.send(:define_method, 'rows') do |*args, &block|
				self.row_vectors *args, &(block.nil? ? Proc.new{} : block)
			end
			Matrix.send(:define_method, 'columns') do |*args, &block|
				self.column_vectors *args, &(block.nil? ? Proc.new{} : block)
			end
			$_matrix_called = true
		end
		Matrix[*self]
		# Creates a matrix where each argument is a row.
		# Matrix[ [25, 93], [-1, 66] ]
		#   =>  25 93
		#		-1 66
		# m = Matrix.build(2, 4) {|row, col| col - row }
		#  => Matrix[[0, 1, 2, 3], [-1, 0, 1, 2]]
		# m = Matrix.build(3) { rand }
		#  => a 3x3 matrix with random elements
		# Matrix.zero(3)
	end
end
$_file_line_offset = 0
$_last_file_name = ''
def fileLineNum
  if $_last_file_name != $F
    $_file_line_offset = $.
    $_last_file_name = $F
  end
  $. - $_file_line_offset + 1
end

class Numeric
  def to_xlscol
    self == 0 ? '' :
      (((self-1)/26).to_i.to_xlscol) +
      ((self-1)%26 + "A".ord).chr
  end
end

class String
  def to_colnum
    raise "Excel column number overflow on string: #{self}" if self.length > 9
    self.length == 0 ? 0 :
      self[-1].upcase.ord - "A".ord + 1 +
      self[0..-2].to_colnum * 26
  end
  alias_method :contains?, :include?
end
class Range
  alias_method :contains?, :include?
end

class Object
  def in? collection
    collection.contains? self
  end
end

class Regexp
  def inverse
    /^((?!#{self}).)*$/
  end
  def to_proc
    l {|s| self=~s}
  end
end

module Enumerable
  def grepv regex
    grep regex.inverse
  end
  def gsub rx, rp
    self.map{|s| s.gsub rx, rp}
  end
  def grep! regex
    self.replace(self.grep regex)
  end
  def grepv! regex
    self.replace(self.grepv regex)
  end
  
  alias_method :skip, :drop
  alias_method :skip_while, :drop_while
  alias_method :some, :find
end

def run_once
  path = File.expand_path(caller.first)
  unless ($__already_run_block ||= []).include?(path)
    yield
    $__already_run_block << path
  end
  puts "$__already_run_block: #{$__already_run_block.inspect}"
end

# Coalesce operator:
# b = false
#
# x = b._? { 2 } # x should be == false
# x = b._? 2 # x should be == false
class Object
  def _? _ = nil
		if block_given?
			raise "A block was passed to coalesce operator. Do not pass a block because that conflicts with a hash literal. Invoke with a lambda: x._? l{...}"
		end
    self
  end
end
class NilClass
  def _? val = nil
		if block_given?
			raise "A block was passed to coalesce operator. Do not pass a block because that conflicts with a hash literal. Invoke with a lambda: x._? l{...}"
		end
		if val.is_a? Proc
			val.call
		else
			val
		end
  end
end

def pp *args, &block
	if !$_pp_called
		require 'pp'
		$_pp_called = true
	end
	if args.size == 0 && !block.nil?
		shortenl = l {|s| s.size > 50 ? "#{s[0..49]}..." : s}
		b = block.binding
        pp b.local_variables
		return PP.pp b.local_variables.map{|v| "#{v} - #{eval("#{v}.to_s", b).then &shortenl}"}
	end
	PP.pp *args, &block
end

class MatchData
	def os
		rval = {}.os
		self.names.each{|name| rval[name] = self[name.to_sym]}
		return rval
	end
end

class String
    alias_method :ends_with?, :end_with?
    alias_method :starts_with?, :start_with?
end

# r -e 'puts secrand.hex'
# r -e 'puts secrand.base64'
# r -e 'puts secrand.base64 16'
# r -e 'puts secrand.urlsafe_base64 16'
# r -e 'puts secrand.uuid'
def secrand
	if !$_secure_random_called
		require 'securerandom'
		$_secure_random_called = true
	end
	SecureRandom
end

def cd dir
	Dir.chdir dir
end

# usage:
# expected_times = (start_time..end_time).step(1.hour).to_set
# x=now;expected_times = (x..x + 5.days).step(1.day).to_a
# expected_times = (now..).step(1.day).take(5).to_a
module RangeWithStepTime
  def step(step_size = 1, &block)
    return to_enum(:step, step_size) unless block_given?

    # Defer to Range for steps other than durations on times
    return super unless step_size.kind_of? ActiveSupport::Duration

    # Advance through time using steps
    time = self.begin
    op = exclude_end? ? :< : :<=
    while self.end.nil? or time.send(op, self.end)
      yield time
      time = step_size.parts.inject(time) { |t, (type, number)| t.advance(type => number) }
    end

    self
  end
end

# call this to operate on, parse, dates and times, date math
def _init_time
	if !$_init_time_called
		require "time"
		require 'active_support/core_ext/numeric/time.rb'
		Range.prepend(RangeWithStepTime)
		$_init_time_called = true
	end
end

def time = _init_time

def now
	_init_time
	Time.now.utc
end

def parse_time timeString
	_init_time
	if timeString =~ /\d{13}/
		Time.at(0, timeString.to_i, :millisecond)
	elsif timeString =~ /\d{10}/
		Time.at timeString.to_i
	else
		Time.parse timeString
	end
end

class String
    def time
			parse_time self
		end
		alias_method :date, :time
end

class Numeric
	def to_duration
		ActiveSupport::Duration.build(self)
	end
  def durstr
    ActiveSupport::Duration.build(self).inspect
	end
	alias_method :to_dur, :to_duration
end

def include_pg
    if !$_pg_called
        require 'pg'
        $_pg_conns = {}
        $_pg_called = true
    end
end
def get_pg_conn str
  	# connection key example: postgresql://username:password@my.host.com.:5432/other_stuff
    conn_key = 'sql_conn'
    if str.starts_with? '--sql_conn'
        conn_key = str.mylines[0][2..]
    end
    if !$_pg_conns.contains? conn_key
        $_pg_conns[conn_key] = PG::Connection.new(conn_key.env)
    end
    $_pg_conns[conn_key]
end
class String
    def sql
        include_pg
        conn = get_pg_conn self
        conn.exec self
    end
end

class Object
    alias_method :old_tap, :tap
    def tap *args, &block
	    _my_map self.method(:old_tap), *args, &block
    end
    ## save object to a local variable
    ## create a method with backing global variable if variable not already declared
    #def to var_name
    #    if Module.const_defined?(:IRB)
    #        $_to_var_value = self
    #        IRB.conf[:MAIN_CONTEXT].workspace.binding.eval "#{var_name} = $_to_var_value"
    #    else
    #        if $_declared_var_names.nil?
    #            $_declared_var_names = {}.to_set
    #        end
    #        eval "$_backing_#{var_name}=self"
    #        TOPLEVEL_BINDING.eval "#{var_name} = $_backing_#{var_name}"
    #        if !var_name.in? $_declared_var_names
    #            TOPLEVEL_BINDING.eval "def #{var_name}; $_backing_#{var_name} end"
    #            $_declared_var_names << var_name
    #        end
    #    end
    #    self
    #end
end

#class Integer
#  alias_method :old_bitwise_or, :|
#  def | x
#    if x.is_a? Symbol
#      self.to x
#    else
#      old_bitwise_or x
#    end
#  end
#end
#class TrueClass
#  alias_method :old_bitwise_or, :|
#  def | x
#    if x.is_a? Symbol
#      self.to x
#    else
#      old_bitwise_or x
#    end
#  end
#end
#class FalseClass
#  alias_method :old_bitwise_or, :|
#  def | x
#    if x.is_a? Symbol
#      self.to x
#    else
#      old_bitwise_or x
#    end
#  end
#end
#class Object
#  def | var_name
#    to var_name
#  end
#end

class OpenStruct
	def deconstruct_keys(keys) = keys ? to_h.slice(*keys) : to_h
	def slice *args
		retval = {}.os
		for arg in args
			retval[arg] = self[arg]
		end
		retval
	end
end
