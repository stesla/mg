% -*- erlang -*-
{application, mg,
 [{description, "DNA fuzzy mutation map-reduce node"},
  {vsn, "0.1.0"},
  {modules, [mg,
             mg_app,
             mg_import,
             mg_query_sup,
             mg_request_sup,
             mg_sup,
             mg_util,
             mg_worker_sup]},
  {registered, []},
  {applications, [kernel, stdlib]},
  {env, []},
  {mod, {mg_app, []}}]}.