SOURCE = "src/Example.j"

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
  system <<-BASH
    javac -d ./bin src/Runtime.java;
    java -jar lib/jasmin.jar -d ./bin "#{SOURCE}";
  BASH
end

task :execute do
  Dir.chdir 'bin' do
    puts "Result: " << system("java #{File.basename(SOURCE, ".j")}").inspect
  end
end

task :default => [:grammarcompile, :compile, :execute]
