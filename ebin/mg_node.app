% -*- erlang -*-
{application, mg_node,
 [{description, "DNA fuzzy mutation map-reduce node"},
  {vsn, "0.1.0"},
  {modules, [mg_node,
             mg_node_app,
             mg_node_import,
             mg_node_sup,
             mg_node_util]},
  {registered, []},
  {applications, [kernel, stdlib]},
  {env, []},
  {mod, {mg_node_app, []}}]}.
