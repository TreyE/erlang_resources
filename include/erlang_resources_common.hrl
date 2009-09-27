-define(TESTING, true).

-ifdef(TESTING).
-define(TEST, true).
-include("eunit.hrl").
-endif.
