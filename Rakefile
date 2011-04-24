SOURCE = File.expand_path('code.jl')
TARGET = SOURCE.gsub(/\.jl\z/, '.j')

desc "Recompile the grammar and make the proper adjustments"
task :grammarcompile do
  Dir.chdir 'src/javalette' do
    system <<-BASH
      bnfc javalette.cf;
      rm *.bak;
      alex *.x;
      happy *.y;
      sed -e 's/Absjavalette/AST/g' -e 's/Lexjavalette/Lexer/g' -e 's/Parjavalette/Parser/g' -e 's/Printjavalette/Printer/g' Absjavalette.hs >../AST.hs
      sed -e 's/Absjavalette/AST/g' -e 's/Lexjavalette/Lexer/g' -e 's/Parjavalette/Parser/g' -e 's/Printjavalette/Printer/g' Lexjavalette.hs >../Lexer.hs
      sed -e 's/Absjavalette/AST/g' -e 's/Lexjavalette/Lexer/g' -e 's/Parjavalette/Parser/g' -e 's/Printjavalette/Printer/g' Parjavalette.hs >../Parser.hs 
      sed -e 's/Absjavalette/AST/g' -e 's/Lexjavalette/Lexer/g' -e 's/Parjavalette/Parser/g' -e 's/Printjavalette/Printer/g' Printjavalette.hs >../Printer.hs      
    BASH
  end
end

desc "Test the code in a very budget way!"
task :luffartest do
  system 'javac -d ./bin src/Runtime.java'
  result = system [
    'cd src',
    "runghc jlc.hs '#{SOURCE}' > '#{TARGET}'",
    %Q{java -jar ../lib/jasmin.jar -d ../bin '#{TARGET}'},
    "cd ../bin",
    "java #{File.basename(SOURCE, ".jl").capitalize}",
  ].join(" && ")
  
  puts
  puts "Result: #{result}"
end

desc "Compile the JLC compiler and put it in bin/"
task :compile do
  Dir.chdir 'src' do
    system 'ghc --make jlc.hs && mv ./jlc ../bin/'
  end
  
  Dir.chdir 'tester' do
    system 'make && mv ./Grade ../bin/'
  end
end

desc "Test the implementation using the built-in testing thingies"
task :test => :compile do
  system '[[ -d tmp/ ]] && rm -r tmp'
  system [
    'mkdir tmp',
    'cp bin/jlc tmp/jlc',
    './bin/Grade tester/ tmp/'
  ].join(" && ")
end

task :default => [:compile]
