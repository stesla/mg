-module(import).

-export([from_file/2]).

-include("tag.hrl").

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
  transform(list_to_tuple([tag | Tokens])).

transform(T) ->
  T#tag{
    position = erlang:list_to_integer(T#tag.position),
    strand = case T#tag.strand of
               "-" -> up;
               "+" -> down
             end,
    length = erlang:list_to_integer(T#tag.length),
    repeat = case T#tag.repeat of
               "0" -> false;
               "1" -> true
             end,
    mmei = case T#tag.mmei of
             "NA" -> no;
             Number -> {yes, erlang:list_to_integer(Number)}
           end}.
