%%%-------------------------------------------------------------------
%%% @author olegb
%%% @copyright (C) 2021, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 26. mar. 2021 09:23
%%%-------------------------------------------------------------------
-module(coffee_machine_server).
-author("olegb").

-behaviour(gen_server).

%% API
-export([start_link/0, espresso/0, cappuccino/0, water/0, add_sugar/1, supply/0, stop/0]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2,
  code_change/3]).

-define(SERVER, ?MODULE).

-record(state, {state, request, coffee, water, sugar, milk}).

%%%===================================================================
%%% API
%%%===================================================================

%% @doc Spawns the server and registers the local name (unique)
-spec(start_link() ->
  {ok, Pid :: pid()} | ignore | {error, Reason :: term()}).
start_link() ->
  gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

espresso() ->
  gen_server:call(?SERVER, espresso).

cappuccino() ->
  gen_server:call(?SERVER, cappuccino).

water() ->
  gen_server:call(?SERVER, water).

add_sugar(AddSugar) ->
  gen_server:call(?SERVER, {add_sugar, AddSugar}).

supply() ->
  gen_server:call(?SERVER, supply).

stop() ->
  gen_server:cast(?SERVER, stop).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

%% @private
%% @doc Initializes the server
-spec(init(Args :: term()) ->
  {ok, State :: #state{}} | {ok, State :: #state{}, timeout() | hibernate} |
  {stop, Reason :: term()} | ignore).
init([]) ->
  StateData = #state{state = idle, coffee = 1000, water = 2000, sugar = 500, milk = 2000},
  io:format("Resources at the moment: ~nCoffee: ~p~nWater: ~p~nSugar: ~p~nMilk: ~p~n", [1000, 2000, 500, 2000]),
  io:format("Select from list: esspresso, cappuccino or water~n"),
  {ok, StateData}.

%% @private
%% @doc Handling call messages
-spec(handle_call(Request :: term(), From :: {pid(), Tag :: term()},
    State :: #state{}) ->
  {reply, Reply :: term(), NewState :: #state{}} |
  {reply, Reply :: term(), NewState :: #state{}, timeout() | hibernate} |
  {noreply, NewState :: #state{}} |
  {noreply, NewState :: #state{}, timeout() | hibernate} |
  {stop, Reason :: term(), Reply :: term(), NewState :: #state{}} |
  {stop, Reason :: term(), NewState :: #state{}}).
handle_call(Request, _From, State = #state{state = MachineState}) when MachineState =:= idle ->
  case Request of
    espresso -> NewState = State#state{state = preparing, request = espresso},
      io:format("Do you want sugar?~n");
    cappuccino -> NewState = State#state{state = preparing, request = cappuccino},
      io:format("Do you want sugar?~n");
    water ->
      NewData = #state{coffee = Coffee, water = Water, sugar = Sugar, milk = Milk} = get_remained_resource(no, State#state{state = preparing, request = water}),
      NewState = get_next_state(NewData),
      io:format("Your drink is preparing, please wait...~n"),
      timer:sleep(1000),
      io:format("Your drink is ready...~n"),
      io:format("Resources at the moment: ~nCoffee: ~p~nWater: ~p~nSugar: ~p~nMilk: ~p~n", [Coffee, Water, Sugar, Milk]);
    _ -> io:format("Not found"), NewState = State
  end,
  {reply, ok, NewState};

handle_call({add_sugar, AddSugar}, _From, State = #state{state = MachineState}) when MachineState =:= preparing ->
  io:format("Your drink is preparing, please wait...~n"),
  NewData = #state{coffee = Coffee, water = Water, sugar = Sugar, milk = Milk} = get_remained_resource(AddSugar, State),
  NewState = get_next_state(NewData),
  timer:sleep(2000),
  io:format("Your drink is ready...~n"),
  io:format("Resources at the moment: ~nCoffee: ~p~nWater: ~p~nSugar: ~p~nMilk: ~p~n", [Coffee, Water, Sugar, Milk]),
  {reply, ok, NewState};

handle_call(supply, _From, #state{state = MachineState}) when  MachineState =:= need_maintenance->
  NewState = #state{state = idle, coffee = 1000, water = 2000, sugar = 500, milk = 2000},
  io:format("Resources at the moment: ~nCoffee: ~p~nWater: ~p~nSugar: ~p~nMilk: ~p~n", [1000, 2000, 500, 2000]),
  io:format("Resources are added, Coffee Machine is ready for offering you a drink!~n"),
  io:format("Select from list: esspresso, cappuccino or water~n"),
  {reply, ok, NewState};

handle_call(_Request, _From, State =#state{state = MachineState}) when  MachineState =:= need_maintenance->
  io:format("Coffee Machine need maintenance, please add resources!~n"),
  {reply, ok, State};

handle_call(_Request, _From, State =#state{}) ->
  io:format("You put a wrong command!~n"),
  {reply, ok, State}.

%% @private
%% @doc Handling cast messages
-spec(handle_cast(Request :: term(), State :: #state{}) ->
  {noreply, NewState :: #state{}} |
  {noreply, NewState :: #state{}, timeout() | hibernate} |
  {stop, Reason :: term(), NewState :: #state{}}).
handle_cast(stop, State = #state{}) ->
  {stop, normal, State};

handle_cast(_Request, State = #state{}) ->
  {noreply, State}.

%% @private
%% @doc Handling all non call/cast messages
-spec(handle_info(Info :: timeout() | term(), State :: #state{}) ->
  {noreply, NewState :: #state{}} |
  {noreply, NewState :: #state{}, timeout() | hibernate} |
  {stop, Reason :: term(), NewState :: #state{}}).
handle_info(_Info, State = #state{}) ->
  {noreply, State}.

%% @private
%% @doc This function is called by a gen_server when it is about to
%% terminate. It should be the opposite of Module:init/1 and do any
%% necessary cleaning up. When it returns, the gen_server terminates
%% with Reason. The return value is ignored.
-spec(terminate(Reason :: (normal | shutdown | {shutdown, term()} | term()),
    State :: #state{}) -> term()).
terminate(_Reason, _State = #state{}) ->
  ok.

%% @private
%% @doc Convert process state when code is changed
-spec(code_change(OldVsn :: term() | {down, term()}, State :: #state{},
    Extra :: term()) ->
  {ok, NewState :: #state{}} | {error, Reason :: term()}).
code_change(_OldVsn, State = #state{}, _Extra) ->
  {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================

get_remained_resource(AddSugar, State = #state{request = Request, coffee = Coffee, water = Water, sugar = Sugar, milk = Milk}) ->
  case Request of
    espresso ->
      case AddSugar of
        yes -> State#state{coffee = Coffee - 50, water = Water - 100, sugar = Sugar - 5};
        no -> State#state{coffee = Coffee - 50, water = Water - 100, milk = Milk}
      end;
    cappuccino ->
      case AddSugar of
        yes -> State#state{coffee = Coffee - 25, milk = Milk - 100, sugar = Sugar - 5};
        no -> State#state{coffee = Coffee - 25, milk = Milk - 950}
      end;
    water -> State#state{water = Water - 100}
  end.

get_next_state(NewState) ->
  if
    (NewState#state.coffee < 100) or (NewState#state.water < 200) or (NewState#state.milk < 200) ->
      NewState#state{state = need_maintenance};
    true ->
      NewState#state{state = idle}
  end.
