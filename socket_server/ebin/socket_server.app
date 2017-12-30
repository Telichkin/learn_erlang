{
  application,
  socket_server,
  [
    {vsn, "1.0.0"},
    {description, "A small tcp-server"},
    {modules, [
      socket_server,
      socket_server_sup,
      socket_server_server
    ]},
    {applications, [
      stdlib,
      kernel
    ]},
    {registered, [
      socket_server
    ]},
    {mod, {socket_server, []}},
    {env, [
      {port, 8080}
    ]}
  ]
}.