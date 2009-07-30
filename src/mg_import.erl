-module(mg_import).

-export([from_file/2]).

-record(tag, {
          sequence,   % string()
          chromosome, % string()
          position,   % integer()
          strand,     % up | down
          length,     % integer()
          repeat,     % true | false
          mmei        % no | {yes, integer()}
          }).
        
from_file(File, Fun) ->
  {ok, Io} = file:open(File, [read]),
  from_io(Io, Fun).

from_io(Io, Fun) ->
  case io:get_line(Io, "") of
    eof ->
      ok;
    {error, Reason} ->
      exit(Reason);
    Data ->
      Fun(parse(Data)),
      from_io(Io, Fun)
  end.

parse(Data) ->
  Tokens = string:tokens(Data, " \t\n"),
  transform(Tokens).

transform([Sequence, Chromosome, Position, Strand, Length, Repeat, Mmei]) ->
  #tag{
    sequence = Sequence,
    chromosome = Chromosome,
    position = erlang:list_to_integer(Position),
    strand = case Strand of
               "-" -> up;
               "+" -> down
             end,
    length = erlang:list_to_integer(Length),
    repeat = case Repeat of
               "0" -> false;
               "1" -> true
             end,
    mmei = case Mmei of
             "NA" -> no;
             Number -> {yes, erlang:list_to_integer(Number)}
           end}.
