-module(restarter).
-export([on_exit/2, test/0]).

%
% API
%
on_exit(Fun, Pid) when is_pid(Pid) ->
  spawn(fun() ->
    process_flag(trap_exit, true),
    link(Pid),
    loop(Fun)
  end);
on_exit(Fun, Starter) when is_function(Starter) ->
  spawn(fun() ->
    process_flag(trap_exit, true),
    spawn_link(Starter),
    loop(Fun)
  end).

%
% Internal Functions
%
loop(Fun) ->
  receive
    {'EXIT', Crasher, Reason} ->
      Fun(Crasher, Reason),
      loop(Fun);
    _ ->
      ok
  end.

%
% Unit Tests
%
test() ->
  ok = test_pid(),
  ok = test_fun(),
  ok.

test_pid() ->
  TestPid  = self(),
  Notifier = fun(_Crasher, _Reason) -> TestPid ! it_crashed end,
  Crasher  = spawn(fun() -> timer:sleep(1000), 1/0 end),
  on_exit(Notifier, Crasher),
  receive it_crashed -> ok
  after   1500       -> false
  end.

test_fun() ->
  TestPid  = self(),
  Notifier = fun(_Crasher, _Reason) -> TestPid ! it_crashed end,
  Crasher  = fun() -> timer:sleep(1000), 1/0 end,
  on_exit(Notifier, Crasher),
  receive it_crashed -> ok
  after   1500       -> false
  end.

