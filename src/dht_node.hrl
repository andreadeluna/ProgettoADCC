-record(node,
{node_id,
  routing_table,
  datastore,
  socket,
  active_nodes}
).

-record(node_info,
{id,
  ip,
  port}
).