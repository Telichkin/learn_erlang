{
  application,                %% <- Required atom
  otp_pool,                   %% <- Application name (should be an atom)
  [
    {vsn, "1.0.0"},           %% <- Application version
    {description, "Run enqueue different concurrent tasks"},
    {modules, [               %% <- Contains a list of all the modules
      otp_pool,               %%    that your application introduces to the system.
      otp_pool_sup,           %%    If you're using a standard OTP structure and are
      otp_pool_server,        %%    using a build tool like rebar3, this is handled for you.
      otp_pool_app_sup,
      otp_pool_worker_sup
    ]},

    {applications, [          %% <- The list of all applications that should be started before our app.
      stdlib,
      kernel
    ]},

    {registered, [            %% <- Contains a list of all the names registered by
      otp_pool                %%    the application. This lets OTP know when
                              %%    there will be name clashes when you try to
                              %%    bundle a bunch of applications together
    ]},

    {mod, {otp_pool, []}}     %% <- Defines a callback module for the 'application',
                              %%    using the application behaviour. Without this tuple
                              %%    our application will be a library application like
                              %%    Erlang stdlib.
  ]
}.