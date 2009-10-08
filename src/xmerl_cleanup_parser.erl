-module(xmerl_cleanup_parser).

-export(['#xml-inheritance#'/0]).

-export(['#root#'/4,
         '#element#'/5,
         '#text#'/1]).

-include("erlang_resources_common.hrl").
-include_lib("xmerl/include/xmerl.hrl").

'#xml-inheritance#'() -> [].


%% The '#text#' function is called for every text segment.

'#text#'(Text) -> Text.

'#root#'(Data, _Attrs, [], _E) -> Data.

'#element#'(_Tag, _Data, Attrs, _Parents, #xmlElement{} = E) ->
Contents = E#xmlElement.content,
NewContent = case has_children(Contents) of
  empty -> [];
  false -> cast_text(maptext(Contents, []), Attrs);
  true -> lists:map(
    fun process_element/1, 
    lists:filter(fun no_text/1, E#xmlElement.content)
  )
end,
E#xmlElement{
  content = NewContent
}.

cast_text(Text, Attrs) ->
  case xmerl_lib:find_attribute(type, Attrs) of
    {value, "integer"} -> integer_cast(Text);
    _ -> list_to_binary(Text) 
  end.

process_element(#xmlElement{name = Name, attributes = Attrs, parents = Parents, content = Contents} = E) ->
  '#element#'(Name, Contents, Attrs, Parents, E).

maptext([#xmlText{value = TVal}|Rest], Acc) -> maptext(Rest, TVal ++ Acc);
maptext([], Acc) -> Acc.

has_children([]) -> empty;
has_children(Contents) -> lists:any(fun is_element/1, Contents).

is_element(#xmlElement{}) -> true;
is_element(_) -> false.

no_text(#xmlText{}) -> false;
no_text(_) -> true.

integer_cast(String) ->
  {Int, _} = string:to_integer(String),
  Int.

-ifdef(TESTING).
simple_xml_in_test() ->
  XmlStringWithDecl = "<?xml version=\"1.0\"?>\n\t <rootnode>\n<fred>\njake\t\n</fred>\n</rootnode>",
  {XmerlStructure, _} = xmerl_scan:string(XmlStringWithDecl),
  [CleanStructure] = xmerl:export_simple([XmerlStructure], xmerl_cleanup_parser),
  FNode = lists:nth(1, CleanStructure#xmlElement.content),
  TNode = FNode#xmlElement.content,
  ?assertEqual(fred, FNode#xmlElement.name),
  ?assertEqual(<<"\njake\t\n">>, TNode).
-endif.
