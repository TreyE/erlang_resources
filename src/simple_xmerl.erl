%%%----------------------------------------------------------------
%%% @author Trey Evans <lewis.r.evans@gmail.com>
%%% @copyright 2009 Trey Evans
%%% @end
%%%----------------------------------------------------------------
-module(simple_xmerl).

-export([xml_in/1, xml_out/1]).

-include("erlang_resources_common.hrl").
-include_lib("xmerl/include/xmerl.hrl").

%% @type
%% dictionary() = term(). as returned by dict:new()
%% element() = term(). as expected by xmerl:export_simple_content()


%%--------------------------------------------------------------------
%% @doc
%% Produces an XML document from a data structure.
%%
%% @spec xml_out(Data) -> dictionary()
%%          Data = element()
%% @end
xml_out(Data) ->
  lists:flatten( 
    xmerl:export_simple_content([Data], xmerl_xml)
  ).


%%--------------------------------------------------------------------
%% @doc
%% Parses an xml document into a dictionary, ignoring the root
%% element name.
%%
%% @spec xml_in(XmlStr) -> dictionary()
%%          XmlStr = string() | binary()
%% @end

xml_in(XmlStr) when is_binary(XmlStr) ->
  xml_in(binary_to_list(XmlStr));
xml_in(XmlStr) when is_list(XmlStr) ->
  case xmerl_scan:string(XmlStr) of
  {XmerlStructure, _ } -> 
    [CleanStructure] = xmerl:export_simple([XmerlStructure], xmerl_cleanup_parser),
      process_structure(CleanStructure);
  _ -> ok
  end.

process_structure(#xmlElement{content = Contents}) -> 
  case children_type(Contents) of
      empty -> dict:new();
      false -> Contents;
      true -> dictify(
          lists:map(fun process_node/1, Contents),
          dict:new()
        )
  end.

dictify([{Key, Value}|Rest], Dict) ->
  case dict:find(Key, Dict) of
    {ok, FVal} when is_list(FVal) -> dictify(Rest, dict:append(Key, Value, Dict));
    {ok, FVal} -> dictify(Rest, dict:store(Key, [FVal, Value], Dict));
    error -> dictify(Rest, dict:store(Key, Value, Dict))
  end;
dictify([], Dict) -> Dict.

process_node(#xmlElement{} = Ele) ->
  {element_name(Ele), process_structure(Ele)}.

children_type([]) -> empty;
children_type(CList) when is_list(CList) -> lists:any(fun is_not_text_element/1, CList);
children_type(_) -> false.

is_not_text_element(#xmlElement{} = _Ele) -> true;
is_not_text_element(_) -> false.

element_name(#xmlElement{name = Name}) when is_list(Name) -> list_to_atom(Name);
element_name(#xmlElement{name = Name}) -> Name.

-ifdef(TESTING).

simple_xml_in_test() ->
  XmlString = "<rootnode/>",
  XmlStringWithDecl = "<?xml version=\"1.0\"?><rootnode/>",
  ExpectedData = dict:new(),
  ?assertEqual(ExpectedData, xml_in(XmlString)),
  ?assertEqual(ExpectedData, xml_in(XmlStringWithDecl)).

complex_xml_in_test() ->
  XmlStringWithDecl = "<?xml version=\"1.0\"?><rootnode><propertyint type=\"integer\">1</propertyint>\n<propertystring>HI</propertystring></rootnode>",
  ExpectedData = dict:from_list([
    {propertyint, 1},
    {propertystring, <<"HI">>}
  ]),
  ?assertEqual(ExpectedData, xml_in(XmlStringWithDecl)).

more_complex_content_parser_test() ->
  XmlText = "<?xml version='1.0' standalone='yes'?>\n<processes>\n<process>\n<logfile>/tmp/garbiage</logfile>\n<category>Testing</category>\n<commandstring>tail -f /var/log/messages</commandstring>\n</process>\n<process>fred</process>\n<process>jake</process>\n</processes>\n",
  ExpectedData = dict:from_list([
    {
      process, 
      [dict:from_list([
        {logfile, <<"/tmp/garbiage">>},
        {category, <<"Testing">>},
        {commandstring, <<"tail -f /var/log/messages">>}
      ]),
      <<"fred">>, <<"jake">>]
    }
  ]),
  Result = (xml_in(XmlText)),
  ?assertEqual(<<"fred">>, lists:nth(2, dict:fetch(process, Result))),
  ?assertEqual(ExpectedData, Result).
-endif.
