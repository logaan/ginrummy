-module(chat_server).
-behaviour(gen_server).

%% API
-export([start_link/0, subscribe/2, listen/3, unlisten/3, state/1,
         broadcast/2, direct_message/3, refresh/2, test/0 ]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
	 terminate/2, code_change/3]).

% Records
-record(state, {subscribers=[], listeners=[]}).
-record(subscriber, {name, messages=[]}).
-record(listener, {name, process_id}).

%%====================================================================
%% API
%%====================================================================
start_link() ->
  gen_server:start_link(?MODULE, [], []).
subscribe(Name, Pid) ->
  gen_server:cast(Pid, {subscribe, Name}).
listen(Name, ListenerPid, Pid) ->
  gen_server:cast(Pid, {listen, Name, ListenerPid}).
unlisten(Name, ListenerPid, Pid) ->
  gen_server:cast(Pid, {unlisten, Name, ListenerPid}).
state(Pid) ->
  gen_server:call(Pid, state).
broadcast(Message, Pid) ->
  gen_server:cast(Pid, {broadcast, Message}).
direct_message(Name, Message, Pid) ->
  gen_server:cast(Pid, {direct_message, Name, Message}).
refresh(Name, Pid) ->
  direct_message(Name, refresh, Pid).

%%====================================================================
%% gen_server callbacks
%%====================================================================
init([]) ->
  {ok, #state{}}.


handle_call(state, _From, State) ->
  Reply = State,
  {reply, Reply, State};
handle_call(_Request, _From, State) ->
  Reply = ok,
  {reply, Reply, State}.


handle_cast({broadcast, Message}, State = #state{ subscribers=Subscribers }) ->
  AddMessage = fun(Subscriber = #subscriber{ messages=Messages }) ->
    Subscriber#subscriber{ messages=[Message|Messages] }
  end,
  NewState = State#state{ subscribers=lists:map(AddMessage, Subscribers) },
  {noreply, update_listeners(NewState)};

handle_cast({direct_message, Name, Message}, State = #state{ subscribers=Subscribers }) ->
  {value, Subscriber=#subscriber{messages=Messages}} = lists:keysearch(Name, 2, Subscribers),
  NewSubscriber = Subscriber#subscriber{ messages=[Message|Messages] },
  NewSubscribers = lists:keyreplace(Name, 2, Subscribers, NewSubscriber),
  NewState = State#state{ subscribers=NewSubscribers },
  {noreply, update_listeners(NewState)};

handle_cast({listen, Name, ProcessID}, State = #state{ listeners=Listeners }) ->
  NewListener = #listener{ name=Name, process_id=ProcessID },
  NewState = State#state{ listeners=[NewListener|Listeners] },
  {noreply, update_listeners(NewState) };

handle_cast({unlisten, Name, ProcessID}, State = #state{ listeners=Listeners }) ->
  OldListner  = #listener{ name=Name, process_id=ProcessID },
  NewListners = lists:delete(OldListner, Listeners),
  NewState    = State#state{ listeners=NewListners },
  {noreply, NewState };

handle_cast({subscribe, Name}, State = #state{ subscribers=Subscribers }) ->
  NewSubscriber = #subscriber{ name=Name },
  {noreply, State#state{ subscribers=[NewSubscriber|Subscribers] } };

handle_cast(_Msg, State) ->
  {noreply, State}.


handle_info(_Info, State) ->
  {noreply, State}.


terminate(_Reason, _State) ->
  ok.


code_change(_OldVsn, State, _Extra) ->
  {ok, State}.

%%--------------------------------------------------------------------
%%% Internal functions
%%--------------------------------------------------------------------
update_listeners(State = #state{ subscribers=Subscribers, listeners=Listeners }) ->
  spawn(fun() -> [
    Listener#listener.process_id !
    {messages, Subscriber#subscriber.messages} ||
    Subscriber <- Subscribers,
    Listener <- Listeners,
    Subscriber#subscriber.name == Listener#listener.name,
    Subscriber#subscriber.messages =/= []
  ] end),
  ClearDeliveredMail = fun(Subscriber = #subscriber{ name=Name }) ->
    case lists:keymember(Name, 2, Listeners) of
      true  -> Subscriber#subscriber{ messages=[] };
      false -> Subscriber
    end
  end,
  NewSubscribers = lists:map(ClearDeliveredMail, Subscribers),
  State#state{ subscribers=NewSubscribers }.

%%--------------------------------------------------------------------
%%% Tests
%%--------------------------------------------------------------------
test() ->
  Printer = spawn(fun print_messages/0),
  {ok, Pid} = chat_server:start_link(),
  chat_server:subscribe(logan, Pid),
  chat_server:broadcast("hey", Pid),
  chat_server:subscribe(royce, Pid),
  chat_server:broadcast("lol", Pid),
  chat_server:subscribe(lyndon, Pid),
  chat_server:broadcast("meow", Pid),
  chat_server:listen(logan, Printer, Pid),
  chat_server:listen(lyndon, Printer, Pid),
  timer:sleep(100),
  chat_server:state(Pid),
  ok.

print_messages() ->
  receive
    _Message -> ok
  end,
  print_messages().
