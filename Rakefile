SOURCE = File.expand_path('code.jl')
TARGET = SOURCE.gsub(/\.jl\z/, '.j')

desc "Recompile the grammar and make the proper adjustments"
task :grammarcompile do
  Dir.chdir 'src/javalette' do
    sh <<-BASH
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
task :luffartest => :compile do
  puts "Result: #{sh './bin/jlc code.jl'}"
end

desc "Compile the JLC compiler and put it in bin/"
task :compile do
  sh 'mkdir -p bin'
  
  sh 'javac -d ./lib src/Runtime.java'
  sh 'llvm-as -o lib/runtime.bc src/Runtime.ll'
  
  Dir.chdir 'src' do
    sh 'make && cp -f ../jlc ../bin'
  end
  
  Dir.chdir 'tester' do
    sh 'make && mv ./Grade ../bin/'
  end
end

desc "Test the implementation using the built-in testing thingies"
task :test => :compile do
  sh 'rm -r tmp'
  sh 'mkdir tmp'
  Dir.chdir 'tmp' do
    sh [
      'cp ../bin/jlc .',
      'cp -r ../lib .',
      'cp ../bin/Grade .',
      './Grade -b LLVM ../tester/ .'
    ].join(" && ")
  end
end

task :default => [:compile]
