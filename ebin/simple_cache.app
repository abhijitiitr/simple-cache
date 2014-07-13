{application, simple_cache,
  [{description, "A simple caching system"},
  {vsn, "0.1.0"},
  {modules, [
    sc_app,
    sc_store,
    sc_element,
    sc_event,
    sc_sup ]},
  {registered,[sc_sup]},
  {env, [{simple_cache, ['a@Abhijits-MacBook-Pro.local','b@Abhijits-MacBook-Pro.local']}]}
  {applications, [kernel, sasl, stdlib,resource_discovery]},
    {mod, {sc_app,[]}},
    {start_phases, []}]}.
