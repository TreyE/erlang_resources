%%%----------------------------------------------------------------
%%% @author Trey Evans <lewis.r.evans@gmail.com>
%%% @copyright 2009 Trey Evans
%%% @end
%%%----------------------------------------------------------------
-module(http_resource_request).

-export([create/2, update/3, delete/2, fetch/2, list_all/1]).

-include("erlang_resources_common.hrl").

-define(HTTP_REQUEST_OPTS, [
{relaxed, true}
]).

-define(REQUEST_OPTS, [
  {full_result, false},
  {body_format, binary}
]).

%% @type resource_id() = string().
%% @type dictionary() = term(). as returned by dict:new()

%%--------------------------------------------------------------------
%% @doc
%% Create a single resource.
%% @spec create(ResourceConfig, Dict) -> dictionary()
%%       ResourceConfig = http_resource_registry:resource_configuration()
%%       Dict = dictionary()
%% @end
%%--------------------------------------------------------------------
create({RUri, RTagName}, Dict) ->
  Body = xmerl_simple:xml_out(RTagName, Dict),
  Uri = all_resources_uri(RUri),
  case make_bodied(Uri, post, Body) of
    {ok, {_, RBody}} -> xmerl_simple:xml_in(RBody);
    A -> A
  end.
  
%%--------------------------------------------------------------------
%% @doc
%% Update a single resource.
%% @spec update(ResourceConfig, RId, Dict) -> ok
%%       ResourceConfig = http_resource_registry:resource_configuration()
%%       RId = resource_id()
%%       Dict = dictionary()
%% @end
%%--------------------------------------------------------------------
update({RUri, RTagName}, RId, Dict) ->
  Body = xmerl_simple:xml_out(RTagName, Dict),
  Uri = single_resource_uri(RUri, RId),
  case make_bodied(Uri, put, Body) of
    {ok, _} -> ok;
    A -> A
  end.

%%--------------------------------------------------------------------
%% @doc
%% Delete a single resource.
%% @spec delete(ResourceConfig, RId) -> dictionary()
%%       ResourceConfig = http_resource_registry:resource_configuration()
%%       RId = resource_id()
%% @end
%%--------------------------------------------------------------------
delete({RUri, _}, RId) ->
  Uri = single_resource_uri(RUri, RId),
  case make_simple_request(Uri, delete) of
    {ok, _} -> ok;
    A -> A
  end.

%%--------------------------------------------------------------------
%% @doc
%% Request a single resource.
%% @spec fetch(ResourceConfig, RId) -> dictionary()
%%       ResourceConfig = http_resource_registry:resource_configuration()
%%       RID = resource_id()
%% @end
%%--------------------------------------------------------------------
fetch({RUri, _}, RId) ->
  Uri = single_resource_uri(RUri, RId),
  case make_simple_request(Uri, get) of
    {ok, {_, Body}} -> xmerl_simple:xml_in(Body);
    A -> A
  end.


%%--------------------------------------------------------------------
%% @doc
%% Request a full list of resources.
%% @spec list_all(ResourceConfig) -> dictionary()
%%       ResourceConfig = http_resource_registry:resource_configuration()
%% @end
%%--------------------------------------------------------------------
list_all({RUri, _}) ->
    {ok, {_, Body}} = make_simple_request(all_resources_uri(RUri), get),
    xmerl_simple:xml_in(Body).

make_simple_request(Url, Method) ->
  http:request(Method, {uri:to_string(Url), [{"User-Agent", "erlang"}]}, ?HTTP_REQUEST_OPTS, ?REQUEST_OPTS).

make_bodied(Url, Method, Body) ->
  http:request(Method, {uri:to_string(Url), [{"User-Agent", "erlang"}, {"Content-type", "application/xml"}], "application/xml", Body}, ?HTTP_REQUEST_OPTS, ?REQUEST_OPTS).

single_resource_uri(BUri, Id) ->
  append_uri_list(
    clean_base(BUri),
    [{"/", Id}, {".", "xml"}]
  ).

all_resources_uri(BUri) ->
  append_uri_list(
    clean_base(BUri),
    [{".", "xml"}]
  ).

append_uri_list(Uri, [{Sep, SVal}|Rest]) ->
  append_uri_list(append_to_path(Uri, Sep, SVal),Rest);
append_uri_list(Uri, []) -> Uri.

append_to_path(Uri, Sep, SVal) ->
  CPath = uri:path(Uri),
  uri:path(
    Uri,
    string:join(
      [CPath, SVal],
      Sep
    )
  ).

clean_base(Uri) ->
  case uri:path(Uri) of
    "/" -> uri:path(Uri, "");
    _ -> Uri
  end.

-ifdef(TESTING).
  all_resources_uri_test() ->
    ExpectedString = "http://testing-resources:8080/resource.xml",
    ResultString = uri:to_string(all_resources_uri(uri:from_string("http://testing-resources:8080/resource"))),
    ?assertEqual(ExpectedString, ResultString).

  single_resource_uri_test() ->
    ExpectedString = "http://testing-resources:8080/resource/523.xml",
    ResultString = uri:to_string(single_resource_uri(uri:from_string("http://testing-resources:8080/resource"), "523")),
    ?assertEqual(ExpectedString, ResultString).


-ifdef(TESTINGHTTP).
  simple_list_test() ->
    Uri = all_resources_uri(uri:from_string("http://testing-resources/host_users")),
    {ok, {Status, Body}}= make_simple_request(Uri, get),
    Records = xmerl_simple:xml_in(Body),
    RList = dict:fetch("host-user", Records),
    ?assert(is_list(RList)),
    ?assertEqual(200, Status),
    ?assert(is_binary(Body)).
   

  create_test() ->
    VDict = dict:from_list([
      {"account-name", <<"test">>},
      {"host", <<"test">>}
    ]),
    UDict = dict:from_list([
      {"host", <<"somebody">>}
    ]),
    BLoc = {uri:from_string("http://testing-resources/host_users"), "host-user"},
    ObjDict = create(BLoc, VDict),
    RId = integer_to_list(dict:fetch("id", ObjDict)),
    ?assertEqual(<<"test">>, dict:fetch("host", ObjDict)),
    UpObjResp = update(BLoc, RId, UDict),
    ?assertEqual(ok, UpObjResp),
    UpObj = fetch(BLoc, RId),
    ?assertEqual(<<"somebody">>, dict:fetch("host", UpObj)),
    DelObjResp = delete(BLoc, RId),
    ?assertEqual(ok, DelObjResp).
-endif.

-endif.
