%% This is the application resource file (.app file) for the erlang_resources,
%% application.
{application, erlang_resources, 
  [{description, "A resource consumer tool for erlang."},
   {vsn, "0.1.0"},
   {modules, [erlang_resources_app,
              erlang_resources_sup,
              http_resource_request,
              simple_xmerl,
              http_resource_registry,
              xmerl_cleanup_parser]},
   {registered,[erlang_resources_sup, http_resource_registry]},
   {versioned_dependencies, [
     {inets,"1.5", gte},
     {eunit, "2.1", gte}
   ]},
   {applications, [kernel, stdlib, inets]},
   {mod, {erlang_resources_app,[]}},
   {start_phases, []}]}.

