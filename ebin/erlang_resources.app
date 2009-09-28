%% This is the application resource file (.app file) for the erlang_resources,
%% application.
{application, erlang_resources, 
  [{description, "A resource consumer tool for erlang."},
   {vsn, "0.1.0"},
   {modules, [erlang_resources_app,
              erlang_resources_sup,
              http_resource_request,
              xmerl_simple,
              http_resource_registry]},
   {registered,[erlang_resources_sup]},
   {versioned_dependencies, [
     {inets,"1.5", gte}
   ]},
   {applications, [kernel, stdlib, inets]},
   {mod, {erlang_resources_app,[]}},
   {start_phases, []}]}.

