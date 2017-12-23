{
  application,
  erlcount,
  [
    {vsn, "1.0.0"},
    {modules, [
      erlcount,
      erlcount_sup,
      erlcount_lib,
      erlcount_dispatch,
      erlcount_counter
    ]},
    {applications, [         %% <- The list of all applications that should be
      otp_pool               %%    started before our application (erlcount).
    ]},
    {registered, [
      erlcount
    ]},
    {mod, {erlcount, []}},
    {env, [                  %% <- Key/value store for application-specific configuration
      {directory, "."},      %%    variables. These variables will be accessible from all
      {regex, [              %%    the rocesses running within the application
        "if\\s.+->",
        "case\\s.+\\sof"
      ]},
      {max_files, 10}
    ]}
  ]
}.