SOURCE = "src/Testing.j"

task :compile do
  system "./compile #{SOURCE}"
end

task :execute do
  Dir.chdir 'bin' do
    puts "Result: " << system("java #{File.basename(SOURCE, ".j")}").inspect
  end
end

task :default => [:compile, :execute]