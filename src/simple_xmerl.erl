%%%----------------------------------------------------------------
%%% @author Trey Evans <lewis.r.evans@gmail.com>
%%% @copyright 2009 Trey Evans
%%% @end
%%%----------------------------------------------------------------
-module(simple_xmerl).

-export([xml_in/1, xml_out/2]).

-include("erlang_resources_common.hrl").
-include_lib("xmerl/include/xmerl.hrl").

%% @type
%% dictionary() = term(). as returned by dict:new()

%%--------------------------------------------------------------------
%% @doc
%% Parses a dictionary with a tag name out into an xml document.
%%
%% @spec xml_out(TName, Dict) -> string()
%%          TName = string()
%%          Dict = dictionary()
%% @end
%%--------------------------------------------------------------------
xml_out(TName, Dict) ->
  case empty_dict(Dict) of
  false -> lists:flatten(
   xmerl:export_simple(
   [{list_to_atom(TName), tagify_dict( Dict)}],
   xmerl_xml)
  );
  true -> lists:flatten(
   xmerl:export_simple(
   [list_to_atom(TName)],
   xmerl_xml)
  )
  end.

tagify_dict(Dict) ->
  KeyList = dict:fetch_keys(Dict),
  tagify_list(Dict, KeyList, []).

tagify_list(Dict, [Key|Rest], RList) ->
  Val = dict:fetch(Key, Dict),
  NList = case is_list(Val) of
    false -> 
      lists:append(RList, [{list_to_atom(Key), process_to_content(Val)}]);
    true ->
      lists:append(RList, lists:map(tagify_with_name(Key), Val)) 
  end, 
  tagify_list(Dict, Rest, NList);
tagify_list(_, [], RList) -> RList.

tagify_with_name(TName) ->
  fun(X) ->
    {list_to_atom(TName), process_to_content(X)}
  end.

process_to_content(Val) when is_binary(Val) -> [binary_to_list(Val)];
process_to_content(Val) when is_float(Val) -> [float_to_list(Val)];
process_to_content(Val) when is_integer(Val) -> [integer_to_list(Val)];
process_to_content(Val) -> tagify_dict(Val).


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

empty_dict(Dict) -> dict:size(Dict) == 0.

process_structure(#xmlElement{content = Contents} = Ele) -> 
  case children_type(Contents) of
      empty -> dict:new();
      false -> process_text_children(Ele);
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
children_type(Children) when is_binary(Children) -> false;
children_type(CList) -> lists:any(fun is_not_text_element/1, CList).

process_text_children(#xmlElement{content = Contents} = Child) -> 
  case get_attribute_value(type, Child) of 
    "integer" -> integer_cast(binary_to_list(Contents));
    "string" -> Contents;
    undefined -> Contents;
    _ -> Contents
  end.

is_not_text_element(#xmlElement{} = _Ele) -> true;
is_not_text_element(_) -> false.

element_name(#xmlElement{name = Name}) when is_atom(Name) -> atom_to_list(Name);
element_name(#xmlElement{name = Name}) -> Name.

integer_cast(String) -> 
  {Int, _} = string:to_integer(String),
  Int.

get_attribute_value(Name, #xmlElement{attributes = Attrs}) -> 
  AttributesList = lists:filter( attribute_selection_function(Name), Attrs ),
  case length(AttributesList) of
    0 -> undefined;
    _ -> 
      Attr = lists:nth(1, AttributesList),
      Attr#xmlAttribute.value
  end.

attribute_selection_function(Name) ->
  fun(X) ->
    X#xmlAttribute.name == Name
  end.

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
    {"propertyint", 1},
    {"propertystring", <<"HI">>}
  ]),
  ?assertEqual(ExpectedData, xml_in(XmlStringWithDecl)).

more_complex_content_parser_test() ->
  XmlText = "<?xml version='1.0' standalone='yes'?>\n<processes>\n<process>\n<logfile>/tmp/garbiage</logfile>\n<category>Testing</category>\n<commandstring>tail -f /var/log/messages</commandstring>\n</process>\n<process>fred</process>\n<process>jake</process>\n</processes>\n",
  ExpectedData = dict:from_list([
    {
      "process", 
      [dict:from_list([
        {"logfile", <<"/tmp/garbiage">>},
        {"category", <<"Testing">>},
        {"commandstring", <<"tail -f /var/log/messages">>}
      ]),
      <<"fred">>, <<"jake">>]
    }
  ]),
  Result = (xml_in(XmlText)),
  ?assertEqual(ExpectedData, Result).


simple_xml_out_test() ->
  ExpectedString = "<?xml version=\"1.0\"?><root-node23/>",
  Data = dict:new(),
  Result = xml_out("root-node23", Data),
  ?assertEqual(ExpectedString, Result).

complex_xml_out_test() ->
  ExpectedString = "<?xml version=\"1.0\"?><rootnode><propertystring>HI</propertystring><propertyint>1</propertyint></rootnode>",
  Data = dict:from_list([
    {"propertyint", 1},
    {"propertystring", <<"HI">>}
  ]),
  Result = xml_out("rootnode", Data),
  ?assertEqual(ExpectedString, Result).

more_complex_content_out_test() ->
  ExpectedString = "<?xml version=\"1.0\"?><processes><process><logfile>/tmp/garbiage</logfile><commandstring>tail -f /var/log/messages</commandstring><category>Testing</category></process><process>fred</process><process>jake</process></processes>",
  Data = dict:from_list([
    {
      "process", 
      [dict:from_list([
        {"logfile", <<"/tmp/garbiage">>},
        {"category", <<"Testing">>},
        {"commandstring", <<"tail -f /var/log/messages">>}
      ]),
      <<"fred">>, <<"jake">>]
    }
  ]),
  Result = xml_out("processes", Data),
  ?assertEqual(ExpectedString, Result).

-endif.
