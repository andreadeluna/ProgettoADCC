%% Definizione record dht_routing_table
-record(dht_routing_table,
        {node_id,
         buckets}
).

%% Definizione record node_info
-record(node_info,
        {id,
         ip,
         port}
).