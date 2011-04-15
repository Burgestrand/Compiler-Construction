SOURCE = File.expand_path('code.jl')
TARGET = SOURCE.gsub(/\.jl\z/, '.j')

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

task :compile do
  system 'javac -d ./bin src/Runtime.java'
  system [
    'cd src',
    "runghc jlc.hs '#{SOURCE}' > '#{TARGET}'",
    %Q{java -jar ../lib/jasmin.jar -d ../bin '#{TARGET}'},
  ].join(" && ")
end

task :execute do
  Dir.chdir 'bin' do
    puts "Result: " << system("java #{File.basename(SOURCE, ".jl").capitalize}").inspect
  end
end

task :default => [:compile, :execute]
