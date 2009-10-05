main([String]) ->
  application:set_env(sinan, prefix, "/usr/local/erlware"),
  application:set_env(sinan, erts_version, "5.7.2"),
  sinan:start(),
  BRef = sinan:gen_build_ref(),
  PRoot = sin_utils:find_project_root(String),
  Seed = sin_build_config:get_seed(PRoot),
  sin_build_config:start_config(BRef, PRoot, Seed, []),
  eta_engine:run(sinan, build, BRef, sin_hooks:get_hooks_function(PRoot)),
  Apps = lists:map(fun({App, _, _, _}) -> 
      atom_to_list(App)
    end,
    sin_build_config:get_value(BRef, "project.apps")
  ),
  Paths = lists:map(fun(App) -> 
      code:add_pathsa(sin_build_config:get_value(BRef, "apps." ++ App ++ ".code_paths"))
    end,
    Apps
  ),
  Modules = lists:flatten(lists:map( fun(App) ->
    sin_build_config:get_value(BRef, "apps." ++ App ++ ".modules")
  end, 
  Apps)),
  RDir = filename:join(PRoot, "test_reports"),
  lists:map( fun(Module) ->
       eunit:test(Module, [{report, {eunit_surefire,[{dir, RDir}]}}])
    end,
    Modules),
  0.
