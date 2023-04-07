#!/usr/bin/env ruby

# Copyright (C) 2015-2017 Apple Inc. All rights reserved.
#
# Redistribution and use in source and binary forms, with or without
# modification, are permitted provided that the following conditions
# are met:
# 1. Redistributions of source code must retain the above copyright
#    notice, this list of conditions and the following disclaimer.
# 2. Redistributions in binary form must reproduce the above copyright
#    notice, this list of conditions and the following disclaimer in the
#    documentation and/or other materials provided with the distribution.
#
# THIS SOFTWARE IS PROVIDED BY APPLE INC. AND ITS CONTRIBUTORS ``AS IS''
# AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO,
# THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR
# PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL APPLE INC. OR ITS CONTRIBUTORS
# BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR
# CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF
# SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS
# INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN
# CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE)
# ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF
# THE POSSIBILITY OF SUCH DAMAGE.

require "pathname"



def underscore(camel_cased_word)
    return camel_cased_word.to_s unless /[A-Z-]|::/.match?(camel_cased_word)
    word = camel_cased_word.to_s.gsub("::", "/")
    #word.gsub!(/(?:(?<=([A-Za-z\d]))|\b)(#{@acronym_regex})(?=\b|[^a-z])/) { "#{$1 && '_' }#{$2.downcase}" }
    word.gsub!(/([A-Z])(?=[A-Z][a-z])|([a-z\d])(?=[A-Z])/) { ($1 || $2) << "_" }
    word.tr!("-", "_")
    word.downcase!
    word
end


class Opcode
    attr_reader :name, :custom, :overloads
    attr_reader :attributes

    def initialize(name, custom)
        @name = name
        @custom = custom
        @attributes = {}
        unless custom
            @overloads = []
        end
    end

    def masmName
        name[0].downcase + name[1..-1]
    end
end

class Arg
    attr_reader :role, :bank, :width

    def initialize(role, bank, width)
        @role = role
        @bank = bank
        @width = width
    end
    
    def self.widthCode(width)
        if width == "Ptr"
            "pointer_width()"
        else
            "W#{width}"
        end
    end

    def widthCode
        Arg.widthCode(width)
    end
    
    def self.roleCode(role)
        case role
        when "U"
            "Use"
        when "D"
            "Def"
        when "ZD"
            "ZDef"
        when "UD"
            "UseDef"
        when "UZD"
            "UseZDef"
        when "UA"
            "UseAddr"
        when "S"
            "Scratch"
        when "ED"
            "EarlyDef"
        when "EZD"
            "EarlyZDef"
        when "LU"
            "LateUse"
        else
            raise
        end
    end
    
    def roleCode
        Arg.roleCode(role)
    end
    
    def to_s
        "#{role}:#{bank}:#{width}"
    end
end

class Overload
    attr_reader :signature, :forms

    def initialize(signature, forms)
        @signature = signature
        @forms = forms
    end
end

class Kind
    attr_reader :name
    attr_accessor :custom

    def initialize(name)
        @name = name
        @custom = false
    end

    def ==(other)
        if other.is_a? String
            @name == other
        else
            @name == other.name and @custom == other.custom
        end
    end

    def Kind.argKinds(kind)
        if kind == "Addr"
            ["Addr", "Stack", "CallArg"]
        else
            [kind]
        end
    end

    def argKinds
        Kind.argKinds(kind)
    end
end

class Form
    attr_reader :kinds, :altName, :archs

    def initialize(kinds, altName, archs)
        @kinds = kinds
        @altName = altName
        @archs = archs
    end
end

class Origin
    attr_reader :fileName, :lineNumber
    
    def initialize(fileName, lineNumber)
        @fileName = fileName
        @lineNumber = lineNumber
    end
    
    def to_s
        "#{fileName}:#{lineNumber}"
    end
end

class Token
    attr_reader :origin, :string
    
    def initialize(origin, string)
        @origin = origin
        @string = string
    end
    
    def ==(other)
        if other.is_a? Token
            @string == other.string
        else
            @string == other
        end
    end
    
    def =~(other)
        @string =~ other
    end
    
    def to_s
        "#{@string.inspect} at #{origin}"
    end
    
    def parseError(*comment)
        if comment.empty?
            raise "Parse error: #{to_s}"
        else
            raise "Parse error: #{to_s}: #{comment[0]}"
        end
    end
end

def lex(str, fileName)
    fileName = Pathname.new(fileName)
    result = []
    lineNumber = 1
    while not str.empty?
        case str
        when /\A\#([^\n]*)/
            # comment, ignore
        when /\A\n/
            # newline, ignore
            lineNumber += 1
        when /\A([a-zA-Z0-9_]+)/
            result << Token.new(Origin.new(fileName, lineNumber), $&)
        when /\A([ \t\r]+)/
            # whitespace, ignore
        when /\A[,:*\/]/
            result << Token.new(Origin.new(fileName, lineNumber), $&)
        else
            raise "Lexer error at #{Origin.new(fileName, lineNumber).to_s}, unexpected sequence #{str[0..20].inspect}"
        end
        str = $~.post_match
    end
    result
end

def isRole(token)
    token =~ /\A((U)|(D)|(UD)|(ZD)|(UZD)|(UA)|(S)|(ED)|(EZD)|(LU))\Z/
end

def isGF(token)
    token =~ /\A((G)|(F))\Z/
end

def isKind(token)
    token =~ /\A((Tmp)|(Imm)|(BigImm)|(BitImm)|(BitImm64)|(ZeroReg)|(SimpleAddr)|(Addr)|(ExtendedOffsetAddr)|(Index)|(PreIndex)|(PostIndex)|(RelCond)|(ResCond)|(DoubleCond)|(StatusCond)|(SIMDInfo))\Z/
end

def isArch(token)
    token =~ /\A((x86)|(x86_32)|(x86_64_avx)|(x86_64)|(arm)|(armv7)|(arm64e)|(arm64_lse)|(arm64)|(32)|(64))\Z/
end

def isWidth(token)
    token =~ /\A((8)|(16)|(32)|(64)|(Ptr)|(128))\Z/
end

def isKeyword(token)
    isRole(token) or isGF(token) or isKind(token) or isArch(token) or isWidth(token) or
        token == "custom" or token == "as"
end

def isIdentifier(token)
    token =~ /\A([a-zA-Z0-9_]+)\Z/ and not isKeyword(token)
end

class Parser
    def initialize(data, fileName)
        @tokens = lex(data, fileName)
        @idx = 0
    end

    def token
        @tokens[@idx]
    end

    def advance
        @idx += 1
    end

    def parseError(*comment)
        if token
            token.parseError(*comment)
        else
            if comment.empty?
                raise "Parse error at end of file"
            else
                raise "Parse error at end of file: #{comment[0]}"
            end
        end
    end

    def consume(string)
        parseError("Expected #{string}") unless token == string
        advance
    end

    def consumeIdentifier
        result = token.string
        parseError("Expected identifier") unless isIdentifier(result)
        advance
        result
    end

    def consumeRole
        result = token.string
        parseError("Expected role (U, D, UD, ZD, UZD, UA, or S)") unless isRole(result)
        advance
        result
    end

    def consumeBank
        result = token.string
        parseError("Expected bank (G or F)") unless isGF(result)
        advance
        result
    end

    def consumeKind
        result = token.string
        parseError("Expected kind (Imm, BigImm, BitImm, BitImm64, ZeroReg, Tmp, SimpleAddr, Addr, ExtendedOffsetAddr, Index, PreIndex, PostIndex, RelCond, ResCond, DoubleCond, or StatusCond)") unless isKind(result)
        advance
        result
    end

    def consumeWidth
        result = token.string
        parseError("Expected width (8, 16, 32, 64, or 128)") unless isWidth(result)
        advance
        result
    end

    def parseArchs
        return nil unless isArch(token)

        result = []
        while isArch(token)
            case token.string
            when "x86"
                result << "X86"
                result << "X86_64"
            when "x86_32"
                result << "X86"
            when "x86_64"
                result << "X86_64"
            when "x86_64_avx"
                result << "X86_64_AVX"
            when "arm"
                result << "ARM_THUMB2"
                result << "ARM64"
            when "armv7"
                result << "ARM_THUMB2"
            when "arm64"
                result << "ARM64"
            when "arm64e"
                result << "ARM64E"
            when "arm64_lse"
                result << "ARM64_LSE"
            when "32"
                result << "X86"
                result << "ARM_THUMB2"
            when "64"
                result << "X86_64"
                result << "ARM64"
            else
                raise token.string
            end
            advance
        end

        consume(":")
        @lastArchs = result
    end

    def consumeArchs
        result = @lastArchs
        @lastArchs = nil
        result
    end

    def parseAndConsumeArchs
        parseArchs
        consumeArchs
    end

    def intersectArchs(left, right)
        return left unless right
        return right unless left

        left.select {
            | value |
            right.find {
                | otherValue |
                value == otherValue
            }
        }
    end

    def parse
        result = {}
        
        loop {
            break if @idx >= @tokens.length

            if token == "custom"
                consume("custom")
                opcodeName = consumeIdentifier

                parseError("Cannot overload a custom opcode") if result[opcodeName]

                result[opcodeName] = Opcode.new(opcodeName, true)
            else
                opcodeArchs = parseAndConsumeArchs

                opcodeName = consumeIdentifier

                if result[opcodeName]
                    opcode = result[opcodeName]
                    parseError("Cannot overload a custom opcode") if opcode.custom
                else
                    opcode = Opcode.new(opcodeName, false)
                    result[opcodeName] = opcode
                end

                signature = []
                forms = []
                
                if isRole(token)
                    loop {
                        role = consumeRole
                        consume(":")
                        bank = consumeBank
                        consume(":")
                        width = consumeWidth
                        
                        signature << Arg.new(role, bank, width)
                        
                        break unless token == ","
                        consume(",")
                    }
                end

                while token == "/"
                    consume("/")
                    case token.string
                    when "branch"
                        opcode.attributes[:branch] = true
                        opcode.attributes[:terminal] = true
                    when "terminal"
                        opcode.attributes[:terminal] = true
                    when "effects"
                        opcode.attributes[:effects] = true
                    when "return"
                        opcode.attributes[:return] = true
                        opcode.attributes[:terminal] = true
                    else
                        parseError("Bad / directive")
                    end
                    advance
                end

                parseArchs
                if isKind(token)
                    loop {
                        kinds = []
                        altName = nil
                        formArchs = consumeArchs
                        loop {
                            kinds << Kind.new(consumeKind)

                            if token == "*"
                                parseError("Can only apply * to Tmp") unless kinds[-1].name == "Tmp"
                                kinds[-1].custom = true
                                consume("*")
                            end

                            break unless token == ","
                            consume(",")
                        }

                        if token == "as"
                            consume("as")
                            altName = consumeIdentifier
                        end

                        parseError("Form has wrong number of arguments for overload") unless kinds.length == signature.length
                        kinds.each_with_index {
                            | kind, index |
                            if kind.name == "Imm" or kind.name == "BigImm" or kind.name == "BitImm" or kind.name == "BitImm64"
                                if signature[index].role != "U"
                                    parseError("Form has an immediate for a non-use argument")
                                end
                                if signature[index].bank != "G"
                                    parseError("Form has an immediate for a non-general-purpose argument")
                                end
                            end
                            if kind.name == "ZeroReg"
                                if signature[index].role != "U"
                                    parseError("Zero immediate must be a use argument")
                                end
                                if signature[index].bank != "G"
                                    parseError("Zero immediate must be a general-purpose argument")
                                end
                            end
                        }
                        forms << Form.new(kinds, altName, intersectArchs(opcodeArchs, formArchs))

                        parseArchs
                        break unless isKind(token)
                    }
                end

                if signature.length == 0
                    raise unless forms.length == 0
                    forms << Form.new([], nil, opcodeArchs)
                end

                opcode.overloads << Overload.new(signature, forms)
            end
        }

        result
    end
end

$fileName = ARGV[0]

parser = Parser.new(IO::read($fileName), $fileName)
$opcodes = parser.parse

def writeH(filename)
    File.open("#{filename}.rs", "w") {
        | outp |
        
        outp.puts "// Generated by opcode_generator.rb from #{$fileName} -- do not edit!"
        

        yield outp
    }
end

writeH("opcode") {
    | outp |
    outp.puts "#[derive(Clone, Copy, Debug, PartialEq, Eq, Hash, PartialOrd, Ord)]"
    outp.puts "#[repr(i16)] pub enum Opcode {"
    $opcodes.keys.each {
        | opcode |
        outp.puts "    #{opcode},"
    }
    outp.puts "}"

    outp.puts "pub const NUM_OPCODES: usize = #{$opcodes.keys.size};"

}

# From here on, we don't try to emit properly indented code, since we're using a recursive pattern
# matcher.

def matchForms(outp, speed, forms, columnIndex, columnGetter, filter, callback)
    return if forms.length == 0

    if filter[forms]
        return
    end

    if columnIndex >= forms[0].kinds.length
        raise "Did not reduce to one form: #{forms.inspect}" unless forms.length == 1
        callback[forms[0]]
        return
    end
    
    groups = {}
    forms.each {
        | form |
        kind = form.kinds[columnIndex].name
        if groups[kind]
            groups[kind] << form
        else
            groups[kind] = [form]
        end
    }

    if speed == :fast and groups.length == 1
        matchForms(outp, speed, forms, columnIndex + 1, columnGetter, filter, callback)
        return
    end

    outp.puts "match #{columnGetter[columnIndex]} {"
    groups.each_pair {
        | key, value |
        outp.puts "#[cfg(target_pointer_width=\"64\")]" if key == "BitImm64"
        cnt = 0 
        Kind.argKinds(key).each {
            | argKind |
            outp.puts "ArgKind::#{argKind}"
            if cnt != Kind.argKinds(key).length - 1
                outp.puts "|"
            else 
                outp.puts " => {"
            end
            cnt += 1
        }
        matchForms(outp, speed, value, columnIndex + 1, columnGetter, filter, callback)
        
        outp.puts "}"
    }
    outp.puts "_ => {}"
   
    outp.puts "}"
end

def matchInstOverload(outp, speed, inst)
    outp.puts "match #{inst}.kind.opcode {"
    $opcodes.values.each {
        | opcode |
        outp.puts "Opcode::#{opcode.name} => {"
        if opcode.custom
            yield opcode, nil
        else
            needOverloadSwitch = ((opcode.overloads.size != 1) or speed == :safe)
            outp.puts "match #{inst}.args.len() {" if needOverloadSwitch
            opcode.overloads.each {
                | overload |
                outp.puts "#{overload.signature.length} => {" if needOverloadSwitch
                yield opcode, overload
                outp.puts "}" if needOverloadSwitch
            }
            if needOverloadSwitch
                outp.puts "_ => {}"
                outp.puts "}"
            end
        end
        #outp.puts "// Hello!"
        outp.puts "}"
    }
    outp.puts "_ => ()"
    
    outp.puts "}"
end
    
def matchInstOverloadForm(outp, speed, inst)
    matchInstOverload(outp, speed, inst) {
        | opcode, overload |
        if opcode.custom
            yield opcode, nil, nil
        else
            columnGetter = proc {
                | columnIndex |
                "#{inst}.args[#{columnIndex}].kind()"
            }
            filter = proc { false }
            callback = proc {
                | form |
                yield opcode, overload, form
            }
            matchForms(outp, speed, overload.forms, 0, columnGetter, filter, callback)
        end
    }
end

$runTimeArchs = {
    "ARM64_LSE" => "ARM64",
    "X86_64_AVX" => "X86_64"
}
def beginArchs(outp, archs)
    return unless archs
    if archs.empty?
        outp.puts "#if 0"
        return
    end
    compileTime = []
    runTime = []
    archs.each {|arch|
        if $runTimeArchs.has_key? arch
            compileTime << $runTimeArchs[arch]
        else
            compileTime << arch
        end
    }
    if compileTime.empty?
        outp.puts("{")
    else
        outp.puts("#[cfg(any(" + compileTime.map {
                      | arch |
                      "target_arch=\"#{arch.downcase}\""
                  }.join(",") + "))] {")
    end
    outp.puts("if " + archs.map {
                  | arch |
                  "is_#{arch.downcase}()"
              }.join(" || ") + " {")
end

def endArchs(outp, archs)
    return unless archs
    outp.puts "}"
    outp.puts "}"
end

maxNumOperands = 0
$opcodes.values.each {
    | opcode |
    next if opcode.custom
    opcode.overloads.each {
        | overload |
        maxNumOperands = overload.signature.length if overload.signature.length > maxNumOperands
    }
}

formTableWidth = (maxNumOperands + 1) * maxNumOperands / 2

writeH("opcode_utils") {
    | outp |

    
    #outp.puts "inline bool opgenHiddenTruth() { return true; }"
    #outp.puts "template<typename T>"
    #outp.puts "inline T* opgenHiddenPtrIdentity(T* pointer) { return pointer; }"
    #outp.puts "#define OPGEN_RETURN(value) do {\\"
    #outp.puts "    if (opgenHiddenTruth())\\"
    #outp.puts "        return value;\\"
    #outp.puts "} while (false)"

    #outp.puts "template<typename Functor>"
    #outp.puts "ALWAYS_INLINE void Inst::forEachArg(const Functor& functor)"
    #outp.puts "{"
    #outp.puts "switch (kind.opcode) {"
    #$opcodes.values.each {
    #    | opcode |
    #    if opcode.custom
    #        outp.puts "case Opcode::#{opcode.name}:"
    #    end
    #}
    #outp.puts "forEachArgCustom(scopedLambdaRef<EachArgCallback>(functor));"
    #outp.puts "return;"
    #outp.puts "default:"
    #outp.puts "forEachArgSimple(functor);"
    #outp.puts "return;"
    #outp.puts "}"
    #outp.puts "}"
   # 
    #outp.puts "template<typename Func>"
    #outp.puts "ALWAYS_INLINE void Inst::forEachArgSimple(const Func& func)"
    #outp.puts "{"
    #outp.puts "    size_t numOperands = args.size();"
    #outp.puts "    size_t formOffset = (numOperands - 1) * numOperands / 2;"
    #outp.puts "    const uint8_t* formBase = g_formTable + kind.opcode * #{formTableWidth} + formOffset;"
    #outp.puts "    for (size_t i = 0; i < numOperands; ++i) {"
    #outp.puts "        uint8_t form = formBase[i];"
    #outp.puts "        func(args[i], decodeFormRole(form), decodeFormBank(form), decodeFormWidth(form));"
    #outp.puts "    }"
    #outp.puts "}"

    outp.puts "impl Inst {"
    outp.puts "pub fn for_each_arg_simple(&self, mut func: impl FnMut(usize, &Arg, ArgRole, Bank, Width)) {"
    outp.puts "let num_operands = self.args.len();"
    outp.puts "let form_offset = (num_operands.wrapping_sub(1)) * num_operands / 2;"
    
    outp.puts "let form_base = G_FORM_TABLE.as_ptr() as usize + self.kind.opcode as usize * #{formTableWidth} + form_offset;"
    #outp.puts "println!(\"offset is at {} for {:?}\", self.kind.opcode as usize * #{formTableWidth} + form_offset, self.kind.opcode);"
    outp.puts "for i in 0..num_operands {"
    outp.puts "let form = unsafe { *(form_base as *const u8).add(i) };"
    #outp.puts "if self.kind.opcode == Opcode::Branch32 { assert_eq!(decode_form_role(form), ArgRole::Use); }"
    outp.puts "func(i, &self.args[i], decode_form_role(form), decode_form_bank(form), decode_form_width(form));"
    outp.puts "}"
    outp.puts "}"
    outp.puts "}"

    outp.puts "#[inline(always)] pub fn is_valid_form(opcode: Opcode, kinds: &[ArgKind]) -> bool"
    outp.puts "{"
    outp.puts "match opcode {"
    $opcodes.values.each {
        | opcode |
        outp.puts "Opcode::#{opcode.name} => {"
        if opcode.custom
            outp.puts "return #{opcode.name}Custom::is_valid_form_static(kinds);"
        else
            outp.puts "match kinds.len() {"
            opcode.overloads.each {
                | overload |
                outp.puts "#{overload.signature.length} => {"
                columnGetter = proc { | columnIndex | "kinds[#{columnIndex} as usize]" }
                filter = proc { false }
                callback = proc {
                    | form |
                    # This conservatively says that Stack is not a valid form for UseAddr,
                    # because it's only valid if it's not a spill slot. This is consistent with
                    # isValidForm() being conservative and it also happens to be practical since
                    # we don't really use isValidForm for deciding when Stack is safe.
                    overload.signature.length.times {
                        | index |
                        if overload.signature[index].role == "UA"
                            outp.puts "if kinds[#{index} as usize] == ArgKind::Stack {"
                            outp.puts "    return false; }"
                        end
                    }
                    
                    notCustom = (not form.kinds.detect { | kind | kind.custom })
                    if notCustom
                        beginArchs(outp, form.archs)
                        outp.puts "return true;"
                        endArchs(outp, form.archs)
                    end
                }
                matchForms(outp, :safe, overload.forms, 0, columnGetter, filter, callback)
                outp.puts "}"
            }
            outp.puts "_ => {}"
            outp.puts "}"
        end
        outp.puts "}"
    }
    outp.puts "_ => {}"
    outp.puts "}"
    outp.puts "return false; "
    outp.puts "}"

    outp.puts "pub const fn is_deinitely_terminal(opcode: Opcode) -> bool"
    outp.puts "{"
    outp.puts "match opcode {"
    didFindTerminals = false
    cnt = 0
    $opcodes.values.each {
        | opcode |
        if opcode.attributes[:terminal]
            outp.puts "Opcode::#{opcode.name} => return true,"
            
        end
    }
    outp.puts("_ => return false }")
    outp.puts "}"

    outp.puts "#[inline] pub fn is_return(opcode: Opcode) -> bool"
    outp.puts "{"
    outp.puts "match opcode {"
    didFindReturns = false
    cnt = 0
    $opcodes.values.each {
        | opcode |
        if opcode.attributes[:return]
            outp.puts "Opcode::#{opcode.name} => return true,"
        end
    }
    outp.puts("_ => return false }")
    outp.puts "}"

}

writeH("opcode_generated") {
    | outp |

    outp.puts "pub(crate) fn print_internal(opcode: Opcode, out: &mut std::fmt::Formatter) -> std::fmt::Result"
    outp.puts "{"
    outp.puts "    match opcode {"
    $opcodes.keys.each {
        | opcode |
        outp.puts "    Opcode::#{opcode} => {"
        outp.puts "        return write!(out, \"#{opcode}\"); }"
    }
    outp.puts "    }"
    outp.puts "}"
    
    outp.puts "pub static G_FORM_TABLE: [u8; #{$opcodes.size * formTableWidth}] = ["
    ix = 0
    $opcodes.values.each {
        | opcode |
        overloads = [nil] * (maxNumOperands + 1)
        unless opcode.custom
            opcode.overloads.each {
                | overload |
                overloads[overload.signature.length] = overload
            }
        end
        
        (0..maxNumOperands).each {
            | numOperands |
            overload = overloads[numOperands]
            if overload
                outp.puts "// #{opcode.name} #{overload.signature.join(', ')}"
                numOperands.times {
                    | index |
                    arg = overload.signature[index]
                    outp.print "encode_inst_form(ArgRole::#{arg.roleCode}, #{arg.bank}P, #{arg.widthCode}), /* #{ix} */"
                    ix += 1
                }
            else
                outp.puts "// Invalid: #{opcode.name} with numOperands = #{numOperands}"
                numOperands.times {
                    outp.print "INVALID_INST_FORM /* #{ix} */, "
                    ix += 1
                }
            end
            outp.puts
        }
    }
    outp.puts "];"

    
    outp.puts "impl Inst {"
    outp.puts "pub fn is_valid_form(&self, code: &Code<'_>) -> bool"
    outp.puts "{"
    matchInstOverloadForm(outp, :safe, "self") {
        | opcode, overload, form |
        if opcode.custom
            outp.puts "return #{opcode.name}Custom::is_valid_form(self, code);"
        else
            beginArchs(outp, form.archs)
            needsMoreValidation = false
            overload.signature.length.times {
                | index |
                arg = overload.signature[index]
                kind = form.kinds[index]
                needsMoreValidation |= kind.custom

                # Some kinds of Args reqire additional validation.
                case kind.name
                when "Tmp"
                    outp.puts "if !self.args[#{index}].tmp().is_#{arg.bank.downcase}p() {"
                    outp.puts "return false; }"
                when "Imm"
                    outp.puts "if !Arg::is_valid_imm_form(self.args[#{index}].value() as _) {"
                    outp.puts "return false; }"
                when "BitImm"
                    outp.puts "if !Arg::is_valid_bit_imm_form(self.args[#{index}].value() as _) {"
                    outp.puts "return false; }"
                when "BitImm64"
                    outp.puts "if !Arg::is_valid_bit_imm64_form(self.args[#{index}].value() as _) {"
                    outp.puts "return false; }"
                when "SimpleAddr"
                    outp.puts "if !self.args[#{index}].ptr().is_gp() {"
                    outp.puts "return false; }"
                when "Addr"
                    if arg.role == "UA"
                        outp.puts "if self.args[#{index}].is_stack() && code.stack_slot(self.args[#{index}].stack_slot()).is_spill() {"
                        outp.puts "return false;}"
                    end
                    outp.puts "if !Arg::is_valid_addr_form(self.kind.opcode, self.args[#{index}].offset() as _) {"
                    outp.puts "return false; }"
                when "ExtendedOffsetAddr"
                    if arg.role == "UA"
                        outp.puts "if self.args[#{index}].is_stack() && code.stack_slot(self.args[#{index}].stack_slot()).is_spill() {"
                        outp.puts "return false; }"
                    end
                when "Index"
                    outp.puts "if !Arg::is_valid_index_form(self.kind.opcode, self.args[#{index}].scale() as _, self.args[#{index}].offset() as _, Some(#{arg.widthCode})) {"
                    outp.puts "return false; }"
                when "PreIndex"
                    outp.puts "if !Arg::is_valid_increment_index_form(self.args[#{index}].offset() as _) {"
                    outp.puts "return false; }"
                when "PostIndex"
                    outp.puts "if !Arg::is_valid_increment_index_form(args[#{index}].offset() as _) {"
                    outp.puts "return false; }"
                when "BigImm"
                when "RelCond"
                when "ResCond"
                when "DoubleCond"
                when "StatusCond"
                when "ZeroReg"
                when "SIMDInfo"
                else
                    raise "Unexpected kind: #{kind.name}"
                end
            }
            #if needsMoreValidation
            #    outp.puts "if !is#{opcode.name}Valid(self) {"
            #    outp.puts "return false; }"
            #end
            outp.puts "return true;"
            endArchs(outp, form.archs)
        end
    }
    outp.puts "return false;"
    outp.puts "}"

    outp.puts "pub fn admits_stack(&self, arg_index: usize, code: &Code<'_>) -> bool"
    outp.puts "{"
    outp.puts "match self.kind.opcode {"
    $opcodes.values.each {
        | opcode |
        outp.puts "Opcode::#{opcode.name} => {"

        if opcode.custom
            outp.puts "return #{opcode.name}Custom::admits_stack(self, arg_index, code);"
        else
            # Switch on the argIndex.
            outp.puts "match arg_index {"

            numArgs = opcode.overloads.map {
                | overload |
                overload.signature.length
            }.max
            
            numArgs.times {
                | argIndex |
                outp.puts "#{argIndex} => {"

                # Check if all of the forms of all of the overloads either do, or don't, admit an address
                # at this index. We expect this to be a very common case.
                numYes = 0
                numNo = 0
                opcode.overloads.each {
                    | overload |
                    useAddr = (overload.signature[argIndex] and
                               overload.signature[argIndex].role == "UA")
                    overload.forms.each {
                        | form |
                        if form.kinds[argIndex] == "Addr" and not useAddr
                            numYes += 1
                        else
                            numNo += 1
                        end
                    }
                }

                # Note that we deliberately test numYes first because if we end up with no forms, we want
                # to say that Address is inadmissible.
                if numYes == 0
                    outp.puts "{ return false }"
                elsif numNo == 0
                    outp.puts "{ return true }"
                else
                    # Now do the full test.

                    needOverloadSwitch = (opcode.overloads.size != 1)

                    outp.puts "match self.args.len() {" if needOverloadSwitch
                    opcode.overloads.each {
                        | overload |

                        useAddr = (overload.signature[argIndex] and
                                   overload.signature[argIndex].role == "UA")
                        
                        # Again, check if all of them do what we want.
                        numYes = 0
                        numNo = 0
                        overload.forms.each {
                            | form |
                            if form.kinds[argIndex] == "Addr" and not useAddr
                                numYes += 1
                            else
                                numNo += 1
                            end
                        }

                        if numYes == 0
                            # Don't emit anything, just drop to default.
                        elsif numNo == 0
                            outp.puts "#{overload.signature.length} => {" if needOverloadSwitch
                            outp.puts "return true;"
                            outp.puts "}" if needOverloadSwitch
                        else
                            outp.puts "#{overload.signature.length} => {" if needOverloadSwitch

                            # This is how we test the hypothesis that changing this argument to an
                            # address yields a valid form.
                            columnGetter = proc {
                                | columnIndex |
                                if columnIndex == argIndex
                                    "ArgKind::Addr"
                                else
                                    "self.args[#{columnIndex}].kind()"
                                end
                            }
                            filter = proc {
                                | forms |
                                numYes = 0

                                forms.each {
                                    | form |
                                    if form.kinds[argIndex] == "Addr"
                                        numYes += 1
                                    end
                                }

                                if numYes == 0
                                    # Drop down, emit no code, since we cannot match.
                                    true
                                else
                                    # Keep going.
                                    false
                                end
                            }
                            callback = proc {
                                | form |
                                beginArchs(outp, form.archs)
                                outp.puts "return true;"
                                endArchs(outp, form.archs)
                            }
                            matchForms(outp, :safe, overload.forms, 0, columnGetter, filter, callback)

                            outp.puts "}" if needOverloadSwitch
                        end
                    }
                    if needOverloadSwitch
                        outp.puts "_ => ()"
                        outp.puts "}"
                    end
                end
                
                outp.puts "}"
            }
            
            outp.puts "_ => ()"
            outp.puts "}"
        end
        
        outp.puts "}"
    }
    outp.puts "_ => ()";
    outp.puts "}"
    outp.puts "return false;"
    outp.puts "}"
    
   
    outp.puts "pub fn admits_extended_offset_addr(&self, arg_index: usize, code: &Code<'_>) -> bool"
    outp.puts "{"
    outp.puts "match self.kind.opcode {"
    $opcodes.values.each {
        | opcode |
        if opcode.custom
            outp.puts "Opcode::#{opcode.name} => {"
            outp.puts "return #{opcode.name}Custom::admits_extended_offset_addr(self, arg_index, code);"
            outp.puts "}"
        end
    }
    outp.puts "_ => ()"
    outp.puts "}"
    outp.puts "return false;"
    outp.puts "}"


    outp.puts "pub fn is_terminal(&self, code: &Code<'_>) -> bool"
    outp.puts "{"
    outp.puts "match self.kind.opcode {"
    foundTrue = false
    cnt = 0 
    $opcodes.values.each {
        | opcode |
        if opcode.attributes[:terminal]
            outp.puts "Opcode::#{opcode.name} => return true,"
            
        end
    }
   
    $opcodes.values.each {
        | opcode |
        if opcode.custom
            outp.puts "Opcode::#{opcode.name} => {"
            outp.puts "return #{opcode.name}Custom::is_terminal(self, code); }"
        end
    }
    outp.puts "_ => return false"
    outp.puts "}"
    outp.puts "}"
    
    outp.puts "pub fn has_non_arg_control_effects(&self, code: &Code<'_>) -> bool"
    outp.puts "{"
    outp.puts "if self.kind.effects {"
    outp.puts "return true; }"
    outp.puts "match self.kind.opcode {"
    foundTrue = false
    $opcodes.values.each {
        | opcode |
        if opcode.attributes[:effects]
            outp.puts "Opcode::#{opcode.name} => return true,"
            
        end
    }
   
    $opcodes.values.each {
        | opcode |
        if opcode.custom
            outp.puts "Opcode::#{opcode.name} => {"
            outp.puts "return #{opcode.name}Custom::has_non_arg_non_control_effects(self, code);}"
        end
    }
    outp.puts "_ => {"
    outp.puts "return false;}"
    outp.puts "}"
    outp.puts "}"
    
    outp.puts "pub fn has_non_arg_effects(&self, code: &Code<'_>) -> bool"
    outp.puts "{"
    outp.puts "if self.kind.effects {"
    outp.puts "return true; }"
    outp.puts "match self.kind.opcode {"
    $opcodes.values.each {
        | opcode |
        if opcode.attributes[:terminal] or opcode.attributes[:effects]
            outp.puts "Opcode::#{opcode.name} => return true,"
            
        end
    }

    $opcodes.values.each {
        | opcode |
        if opcode.custom
            outp.puts "Opcode::#{opcode.name} => {"
            outp.puts "return #{opcode.name}Custom::has_non_arg_effects(self, code); }"
        end
    }
    outp.puts "_ => {"
    outp.puts "return false; }"
    outp.puts "}"
    outp.puts "}"
    

    outp.puts "pub fn generate(&self, jit: &mut TargetMacroAssembler, context: &mut GenerationContext<'_, '_>) -> Jump"
    outp.puts "{"
    #outp.puts "jit.comment(format!(\"{}\", self));"
    outp.puts "let mut result = Jump::default();"
    matchInstOverloadForm(outp, :fast, "self") {
        | opcode, overload, form |
        if opcode.custom
            outp.puts "return #{opcode.name}Custom::generate(self, jit, context);"
        else
            beginArchs(outp, form.archs)
            if form.altName
                methodName = form.altName
            else
                methodName = opcode.masmName
            end
            
            #if methodName == "move"
            #    methodName = "mov"
            #end
            #if methodName.start_with?("branch") and form.kinds.length == 3
            #    methodName = methodName 
            #elsif methodName.start_with?("moveConditionally")
            #    
            #    if form.kinds.length == 6
            #        methodName = methodName + "ThenElse"
            #    else 
            #        methodName = methodName
            #    end
            #elsif form.kinds.length > 2
            #    methodName = methodName + "_rrr"
            #end
            

            methodName = underscore(methodName)
            
            if opcode.attributes[:branch]
                outp.print "result = "
            end
            outp.print "jit.#{methodName}("

            form.kinds.each_with_index {
                | kind, index |
                if index != 0
                    outp.print ", "
                end
                case kind.name
                when "Tmp"
                    if overload.signature[index].bank == "G"
                        outp.print "self.args[#{index}].gpr()"
                    else
                        outp.print "self.args[#{index}].fpr()"
                    end
                when "Imm", "BitImm"
                    outp.print "self.args[#{index}].as_imm32()"
                when "BigImm"
                    outp.print "self.args[#{index}].as_big_imm()"
                when "BitImm64"
                    outp.print "self.args[#{index}].as_imm64()"
                when "ZeroReg"
                    outp.print "self.args[#{index}].as_zero_reg()"
                when "SimpleAddr", "Addr", "ExtendedOffsetAddr"
                    outp.print "self.args[#{index}].as_address()"
                when "Index"
                    outp.print "self.args[#{index}].as_base_index()"
                when "PreIndex"
                    outp.print "self.args[#{index}].as_pre_index_address()"
                when "PostIndex"
                    outp.print "self.args[#{index}].as_post_index_address()"
                when "RelCond"
                    outp.print "self.args[#{index}].as_relational_condition()"
                when "ResCond"
                    outp.print "self.args[#{index}].as_result_condition()"
                when "DoubleCond"
                    outp.print "self.args[#{index}].as_double_condition()"
                when "StatusCond"
                    outp.print "self.args[#{index}].as_status_condition()"
                when "SIMDInfo"
                    outp.print "todo!(\"SIMDInfo\");)"
                    #outp.print "args[#{index}].simdInfo()"
                end
            }

            outp.puts ");"
            outp.puts "return result;"
            endArchs(outp, form.archs)
        end
    }
    outp.puts "unreachable!(\"Failed to generate: {}\", self)"
    outp.puts "}"
    outp.puts "}"
}

