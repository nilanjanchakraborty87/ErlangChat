-module(mutex).
-export([wait/0, signal/0, start/0, stop/0, init/0]).

start() ->
  register(mutex, spawn(?MODULE, init, [])).

init() ->
  free().

wait() ->
  mutex ! {wait, self()},
  receive ok -> ok end.

signal() ->
  mutex ! {signal, self()}, ok.

busy(Pid) ->
  receive
    {signal, Pid} ->
      free()
  end.

free() ->
  receive
    {wait, Pid} ->
      Pid ! ok,
      busy(Pid);
    stop ->
      terminate()
  end.

stop() ->
  mutex ! stop.

terminate() ->
  receive
    {wait, Pid} ->
      exit(Pid, kill),
      terminate()
  after
    0 -> ok
  end.
