-module(http_resource_request).

-include("erlang_resources_common.hrl").

-define(HTTP_REQUEST_OPTS, [
{relaxed, true}
]).

-define(REQUEST_OPTS, [
  {full_result, false},
  {body_format, binary}
]).

make_request(Url, Method) ->
  http:request(Method, {uri:to_string(Url), [{"User-Agent", "erlang"}]}, ?HTTP_REQUEST_OPTS, ?REQUEST_OPTS).

single_resource_uri(BUri, RName, Id) ->
  append_uri_list(
    clean_base(BUri),
    [{"/", RName}, {"/", Id}, {".", "xml"}]
  ).

all_resources_uri(BUri, RName) ->
  append_uri_list(
    clean_base(BUri),
    [{"/", RName}, {".", "xml"}]
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
    ResultString = uri:to_string(all_resources_uri(uri:from_string("http://testing-resources:8080/"), "resource")),
    ?assertEqual(ExpectedString, ResultString).

  single_resource_uri_test() ->
    ExpectedString = "http://testing-resources:8080/resource/523.xml",
    ResultString = uri:to_string(single_resource_uri(uri:from_string("http://testing-resources:8080"), "resource", "523")),
    ?assertEqual(ExpectedString, ResultString).

  simple_list_test() ->
    Uri = all_resources_uri(uri:from_string("http://testing-resources/"), "host_users"),
    {ok, {Status, Body}}= make_request(Uri, get),
    Records = xmerl_simple:xml_in(Body),
    RList = dict:fetch("host-user", Records),
    ?assert(is_list(RList)),
    ?assertEqual(200, Status),
    ?assert(is_binary(Body)).
   
-endif.
