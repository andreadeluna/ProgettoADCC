%% Definizione record node
-record(node,
{node_id,
  routing_table,
  datastore,
  pid,
  active_nodes}
).

%% Definizione record node_info
-record(node_info,
{id,
  pid}
).