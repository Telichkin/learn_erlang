{
  application,
  erlcount,
  [
    {vsn, "1.0.0"},
    {description, "Run regular expressions on Erlang source files"},
    {modules, [
      erlcount,
      erlcount_sup,
      erlcount_lib,
      erlcount_dispatch,
      erlcount_counter
    ]},
    {applications, [         %% <- The list of all applications that should be
      stdlib,                %%    started before our application (erlcount).
      kernel,
      otp_pool
    ]},
    {registered, [
      erlcount
    ]},
    {mod, {erlcount, []}},
    {env, [                  %% <- Key/value store for application-specific configuration
      {directory, "."},      %%    variables. These variables will be accessible from all
      {regex, [              %%    the resources running within the application
        "if\\s.+->",
        "case\\s.+\\sof"
      ]},
      {max_files, 10}
    ]}
  ]
}.