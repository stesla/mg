require 'rake/clean'

CLEAN.include '**/*.beam'

task :default => [:build_erl]

BEAMS = FileList['src/*.erl'].pathmap('%{src,ebin}X.beam')



task :beams do
  p BEAMS
end

ERLC_FLAGS = '-o ./ebin'
ERLC_TEST_FLAGS = ''

def erlc(t)
  sh "erlc #{ERLC_FLAGS} #{ENV['TEST'] ? ERLC_TEST_FLAGS : ''} #{t.source}"
end

rule ".beam" => "%{ebin,src}X.erl" do |t|
  erlc t
end

task :build_erl => BEAMS


