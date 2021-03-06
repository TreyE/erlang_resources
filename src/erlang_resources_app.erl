%%%----------------------------------------------------------------
%%% @author Trey Evans <lewis.r.evans@gmail.com>
%%% @copyright 2009 Trey Evans
%%% @end
%%% @doc
%%% When starting this application, you may configure 
%%% http_resource definitions to be automatically loaded.
%%% If you set the application environment variable:
%%% {erlang_resources, http_resources} to a set of
%%% {@link resource_settings()}, it will be loaded automatically
%%% when you call application:start/1.
%%% @end
%%%----------------------------------------------------------------
-module(erlang_resources_app).

-behaviour(application).

%% Application callbacks
-export([start/2, stop/1]).

%% @type resource_config_entry() = {string() | atom(), http_resource_registry:resource_configuration()}.
%% A resource configuration entry, to be stored in the registry.
%% @type resource_settings() = [resource_config_entry()].
%% A list of resource configuration entries.

%%%===================================================================
%%% Application callbacks
%%%===================================================================

%%--------------------------------------------------------------------
%% @private
%% @doc
%% This function is called whenever an application is started using
%% application:start/[1,2], and should start the processes of the
%% application. If the application is structured according to the OTP
%% design principles as a supervision tree, this means starting the
%% top supervisor of the tree.
%%
%% @spec start(StartType, StartArgs) -> {ok, Pid} |
%%                                      {ok, Pid, State} |
%%                                      {error, Reason}
%%      StartType = normal | {takeover, Node} | {failover, Node}
%%      StartArgs = term()
%% @end
%%--------------------------------------------------------------------
start(_StartType, _StartArgs) ->
    case erlang_resources_sup:start_link(resource_vals()) of
        {ok, Pid} ->
            {ok, Pid};
        Error ->
            Error
    end.

resource_vals() ->
  case application:get_env(erlang_resources, http_resources) of
    {ok, Val} -> Val;
    undefined -> []
  end.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% This function is called whenever an application has stopped. It
%% is intended to be the opposite of Module:start/2 and should do
%% any necessary cleaning up. The return value is ignored.
%%
%% @spec stop(State) -> void()
%% @end
%%--------------------------------------------------------------------
stop(_State) ->
    ok.

%%%===================================================================
%%% Internal functions
%%%===================================================================


