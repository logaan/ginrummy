%%%-------------------------------------------------------------------
%%% File  : chat_server.erl
%%% Author  : Colin Campbell-McPherson <colin@logaan.net>
%%% Description : 
%%%
%%% Created :  7 Mar 2010 by Colin Campbell-McPherson <colin@logaan.net>
%%%-------------------------------------------------------------------
-module(chat_server).
-define(SERVER, chat_server).
-behaviour(gen_server).

%% API
-export([start_link/0, subscribe/1, listen/2, state/0, broadcast/1, test/0, test2/0]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
	 terminate/2, code_change/3]).

-record(state, {subscribers=[], listeners=[]}).
-record(subscriber, {name, messages=[]}).
-record(listener, {name, process_id}).

%%====================================================================
%% API
%%====================================================================
%%--------------------------------------------------------------------
%% Function: start_link() -> {ok,Pid} | ignore | {error,Error}
%% Description: Starts the server
%%--------------------------------------------------------------------
start_link() ->
  gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).
subscribe(Name) ->
  gen_server:cast(?SERVER, {subscribe, Name}).
listen(Name, Pid) ->
  gen_server:cast(?SERVER, {listen, Name, Pid}).
state() ->
  gen_server:call(?SERVER, state).
broadcast(Message) ->
  gen_server:cast(?SERVER, {broadcast, Message}).

%%====================================================================
%% gen_server callbacks
%%====================================================================

%%--------------------------------------------------------------------
%% Function: init(Args) -> {ok, State} |
%%             {ok, State, Timeout} |
%%             ignore         |
%%             {stop, Reason}
%% Description: Initiates the server
%%--------------------------------------------------------------------
init([]) ->
  {ok, #state{}}.

%%--------------------------------------------------------------------
%% Function: %% handle_call(Request, From, State) -> {reply, Reply, State} |
%%                    {reply, Reply, State, Timeout} |
%%                    {noreply, State} |
%%                    {noreply, State, Timeout} |
%%                    {stop, Reason, Reply, State} |
%%                    {stop, Reason, State}
%% Description: Handling call messages
%%--------------------------------------------------------------------
handle_call(state, _From, State) ->
  Reply = State,
  {reply, Reply, State};
handle_call(_Request, _From, State) ->
  Reply = ok,
  {reply, Reply, State}.

%%--------------------------------------------------------------------
%% Function: handle_cast(Msg, State) -> {noreply, State} |
%%                    {noreply, State, Timeout} |
%%                    {stop, Reason, State}
%% Description: Handling cast messages
%%--------------------------------------------------------------------
handle_cast({broadcast, Message}, State = #state{ subscribers=Subscribers }) ->
  AddMessage = fun(Subscriber = #subscriber{ messages=Messages }) ->
    Subscriber#subscriber{ messages=[Message|Messages] }
  end,
  NewState = State#state{ subscribers=lists:map(AddMessage, Subscribers) },
  {noreply, update_listeners(NewState)};

handle_cast({listen, Name, ProcessID}, State = #state{ listeners=Listeners }) ->
  NewListener = #listener{ name=Name, process_id=ProcessID },
  NewState = State#state{ listeners=[NewListener|Listeners] },
  {noreply, update_listeners(NewState) };

handle_cast({subscribe, Name}, State = #state{ subscribers=Subscribers }) ->
  NewSubscriber = #subscriber{ name=Name },
  {noreply, State#state{ subscribers=[NewSubscriber|Subscribers] } };

handle_cast(_Msg, State) ->
  {noreply, State}.

%%--------------------------------------------------------------------
%% Function: handle_info(Info, State) -> {noreply, State} |
%%                     {noreply, State, Timeout} |
%%                     {stop, Reason, State}
%% Description: Handling all non call/cast messages
%%--------------------------------------------------------------------
handle_info(_Info, State) ->
  {noreply, State}.

%%--------------------------------------------------------------------
%% Function: terminate(Reason, State) -> void()
%% Description: This function is called by a gen_server when it is about to
%% terminate. It should be the opposite of Module:init/1 and do any necessary
%% cleaning up. When it returns, the gen_server terminates with Reason.
%% The return value is ignored.
%%--------------------------------------------------------------------
terminate(_Reason, _State) ->
  ok.

%%--------------------------------------------------------------------
%% Func: code_change(OldVsn, State, Extra) -> {ok, NewState}
%% Description: Convert process state when code is changed
%%--------------------------------------------------------------------
code_change(_OldVsn, State, _Extra) ->
  {ok, State}.

%%--------------------------------------------------------------------
%%% Internal functions
%%--------------------------------------------------------------------
update_listeners(State = #state{ subscribers=Subscribers, listeners=Listeners }) ->
  spawn(fun() -> [
    Listener#listener.process_id !
    {chat_messages, Subscriber#subscriber.name, Subscriber#subscriber.messages}    ||
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
%%% Unit tests
%%--------------------------------------------------------------------
test() ->
  Printer = spawn(fun print_messages/0),
  chat_server:start_link(),
  chat_server:subscribe(logan),
  chat_server:broadcast("hey"),
  chat_server:subscribe(royce),
  chat_server:broadcast("lol"),
  chat_server:subscribe(lyndon),
  chat_server:broadcast("meow"),
  chat_server:listen(logan, Printer),
  chat_server:listen(lyndon, Printer),
  timer:sleep(100),
  chat_server:state().

test2() ->
  Printer = spawn(fun print_messages/0),
  chat_server:listen(royce, Printer).

print_messages() ->
  receive
    Message -> io:format("~p~n", [Message])
  end,
  print_messages().
