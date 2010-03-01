{application, ginrummy,
 [{description, "ginrummy"},
  {vsn, "0.01"},
  {modules, [
    ginrummy,
    ginrummy_app,
    ginrummy_sup,
    ginrummy_web,
    ginrummy_deps
  ]},
  {registered, []},
  {mod, {ginrummy_app, []}},
  {env, []},
  {applications, [kernel, stdlib, crypto]}]}.
