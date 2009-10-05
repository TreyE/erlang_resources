%%%----------------------------------------------------------------
%%% @author Trey Evans <lewis.r.evans@gmail.com>
%%% @copyright 2009 Trey Evans
%%% @end
%%%----------------------------------------------------------------
-module(http_resource_registry).

-behaviour(gen_server).

%% API
-export([start_link/1, store/3, fetch/1, remove/1]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
	 terminate/2, code_change/3]).

-define(SERVER, ?MODULE). 

-include("erlang_resources_common.hrl").

%%%===================================================================
%%% API
%%%===================================================================

%% @type uri() = term().
%% A uri, as from uri:from_string/1.
%% @type resource_uri() = uri().
%% The uri of the resource.
%% @type singular_element_name() = string().
%% The single element name in a resource's xml document.
%% @type resource_configuration() = {resource_uri(), singular_element_name()}


%%--------------------------------------------------------------------
%% @doc
%% Stores a resource configuration
%%
%% @spec store(Name, RUri, TName) -> ok
%%         Name = string() | atom()
%%         RUri = resource_uri()
%%         TName = singular_element_name()
%% @end
store(Name, RUri, TName) ->
  gen_server:call({global, ?SERVER}, {store, Name, {RUri, TName}}).

%%--------------------------------------------------------------------
%% @doc
%% Returns a stored resource configuration.
%%
%% @spec fetch(Name) -> resource_configuration()
%%         Name = string() | atom()
%% @end
fetch(Name) ->
  gen_server:call({global, ?SERVER}, {fetch, Name}).

%%--------------------------------------------------------------------
%% @doc
%% Removes a stored resource configuration.
%%
%% @spec remove(Name) -> ok()
%%         Name = string() | atom()
%% @end
remove(Name) ->
  gen_server:call({global, ?SERVER}, {remove, Name}).

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Starts the server
%%
%% @spec start_link(Args) -> {ok, Pid} | ignore | {error, Error}
%%         Args = [DictElement]
%%         DictElement = {string() | atom(), resource_configuration()}
%% @end
%%--------------------------------------------------------------------
start_link(Args) ->
    gen_server:start_link({global, ?SERVER}, ?MODULE, Args, []).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Initiates the server
%%
%% @spec init(Args) -> {ok, State} |
%%                     {ok, State, Timeout} |
%%                     ignore |
%%                     {stop, Reason}
%% @end
%%--------------------------------------------------------------------
init([]) ->
    {ok, dict:new()};
init(Args)->
    {ok, dict:from_list(Args)}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling call messages
%%
%% @spec handle_call(Request, From, State) ->
%%                                   {reply, Reply, State} |
%%                                   {reply, Reply, State, Timeout} |
%%                                   {noreply, State} |
%%                                   {noreply, State, Timeout} |
%%                                   {stop, Reason, Reply, State} |
%%                                   {stop, Reason, State}
%% @end
%%--------------------------------------------------------------------
handle_call(Request, _From, State) ->
    case Request of
      {fetch, Name} -> {reply, dict:fetch(Name, State), State};
      {store, Name, Spec} -> {reply, ok, dict:store(Name, Spec, State)};
      {remove, Name} -> {reply, ok, dict:erase(Name, State)}
    end.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling cast messages
%%
%% @spec handle_cast(Msg, State) -> {noreply, State} |
%%                                  {noreply, State, Timeout} |
%%                                  {stop, Reason, State}
%% @end
%%--------------------------------------------------------------------
handle_cast(_Msg, State) ->
    {noreply, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling all non call/cast messages
%%
%% @spec handle_info(Info, State) -> {noreply, State} |
%%                                   {noreply, State, Timeout} |
%%                                   {stop, Reason, State}
%% @end
%%--------------------------------------------------------------------
handle_info(_Info, State) ->
    {noreply, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% This function is called by a gen_server when it is about to
%% terminate. It should be the opposite of Module:init/1 and do any
%% necessary cleaning up. When it returns, the gen_server terminates
%% with Reason. The return value is ignored.
%%
%% @spec terminate(Reason, State) -> void()
%% @end
%%--------------------------------------------------------------------
terminate(_Reason, _State) ->
    ok.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Convert process state when code is changed
%%
%% @spec code_change(OldVsn, State, Extra) -> {ok, NewState}
%% @end
%%--------------------------------------------------------------------
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================

-ifdef(TESTING).

init_test() -> 
  ?assertEqual(init([]), {ok, dict:new()}).

init_with_args_test() ->
  IArgs = [{something, fred}],
  ExpectedDict = dict:from_list(IArgs),
  {ok, ExpectedDict} = init(IArgs).

-endif.
