SOURCE = "src/Example.j"

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

task :default => [:compile, :execute]