-module(dht_node).
-include("dht_node.hrl").

-import(dht_routing_table, [get_own_ip/0, get_udp_port/0]).

-export([
  start/0,
  join_network/2,
  stop/1,
  ping/2,
  store/3,
  find_node/2,
  find_value/2,
  send_message/3,
  handle_message/2,
  lookup_node/2,
  print_node/1
]).

start() ->
  Id = generate_random_id(),
  HashedId = hash_id(Id),
  RoutingTable = dht_routing_table:init(HashedId),
  Datastore = dht_datastore:init(),
  Socket = gen_udp:open(0, [{active, true}]),
  Node = #node{
    node_id = HashedId,
    routing_table = RoutingTable,
    datastore = Datastore,
    socket = Socket,
    active_nodes = []
  },
  NodeInfo = #node_info{
    id = HashedId,
    ip = dht_routing_table:get_own_ip(),
    port = dht_routing_table:get_udp_port()
  },
  UpdatedRoutingTable = dht_routing_table:add_node(RoutingTable, NodeInfo),
  UpdatedNode = Node#node{routing_table = UpdatedRoutingTable},
  spawn(fun() -> listen(UpdatedNode) end),
  io:format("~n~nNodo creato con successo ~n~n"),
  UpdatedNode.

generate_random_id() ->
  Bytes = crypto:strong_rand_bytes(8),
  Bytes.

hash_id(Id) ->
  Hash = crypto:hash(sha, Id),
  erlang:phash2(Hash).

join_network(NewNode, ExistingNode) ->
  ExistingNodeInfo = #node_info{
    id = ExistingNode#node.node_id,
    ip = dht_routing_table:get_own_ip(),
    port = dht_routing_table:get_udp_port()
  },
  NewNodeInfo = #node_info{
    id = NewNode#node.node_id,
    ip = dht_routing_table:get_own_ip(),
    port = dht_routing_table:get_udp_port()
  },
  ExistingNodeUpdated = ExistingNode#node{
    node_id = ExistingNode#node.node_id,
    routing_table = dht_routing_table:add_node(ExistingNode#node.routing_table, NewNodeInfo),
    datastore = ExistingNode#node.datastore,
    socket = ExistingNode#node.socket,
    active_nodes = [NewNodeInfo | ExistingNode#node.active_nodes]
  },
  NewNodeUpdated = NewNode#node{
    node_id = NewNode#node.node_id,
    routing_table = dht_routing_table:add_node(NewNode#node.routing_table, ExistingNodeInfo),
    datastore = NewNode#node.datastore,
    socket = NewNode#node.socket,
    active_nodes = [ExistingNodeInfo | NewNode#node.active_nodes]
  },
  {ExistingNodeUpdated, NewNodeUpdated}.

lookup_node(Node, TargetId) ->
  case dht_routing_table:find_node(Node#node.routing_table, TargetId) of
    [] ->
      io:format("~nNodo non trovato~n"),
      {error, not_found};
    [NodeInfo|_] ->
      io:format("~nNodo più vicino trovato~n"),
      lookup_node_recursive(Node, NodeInfo)
  end.

lookup_node_recursive(Node, NodeInfo) ->
  {_, NodeInfoID} = NodeInfo,
  case ping(Node, NodeInfoID#node_info.id) of
    ok ->
      handle_active_node(Node, NodeInfo),
      {ok, NodeInfo};
    {error, timeout} ->
      handle_inactive_node(Node, NodeInfo),
      case dht_routing_table:find_closest_nodes(Node#node.routing_table, NodeInfoID) of
        [] ->
          {error, not_found};
        [NextNode|_] ->
          lookup_node_recursive(Node, NextNode)
      end
  end.

find_node(Node, TargetId) ->
  Nodes = dht_routing_table:find_closest_nodes(Node#node.routing_table, TargetId),
  case Nodes of
    [] ->
      % Nessun nodo trovato, puoi gestire l'errore qui
      {error, node_not_found};
    [{_, ClosestNode} | _RestNodes] ->
      % Prendi l'ID del nodo più vicino e usalo come destinazione
      Reply = {node_reply, Nodes},
      io:format("~nID più vicino: ~p~n", [ClosestNode#node_info.id]),
      send_message(Node, Reply, ClosestNode#node_info.id)
  end.

find_value(Node, Key) ->
  case dht_datastore:lookup(Node#node.datastore, Key) of
    {ok, Value} ->
      Reply = {value_reply, Key, Value},
      send_message(Node, Reply, Node#node.node_id);
    error ->
      Nodes = dht_routing_table:find_closest_nodes(Node#node.routing_table, Node#node.node_id),
      case Nodes of
        [] ->
          % Nessun nodo trovato, puoi gestire l'errore qui
          {error, node_not_found};
        [{_, ClosestNode} | _RestNodes] ->
          % Prendi l'ID del nodo più vicino e usalo come destinazione
          Reply = {node_reply, Nodes},
          io:format("~nID nodo più vicino: ~p~n", [ClosestNode#node_info.id]),
          send_message(Node, Reply, ClosestNode#node_info.id)
      end
  end.

store(Node, Key, Value) ->
  NewDatastore = dht_datastore:store(Node#node.datastore, Key, Value),
  NewNode = Node#node{
    node_id = Node#node.node_id,
    routing_table = Node#node.routing_table,
    datastore = NewDatastore,
    socket = Node#node.socket,
    active_nodes = Node#node.active_nodes
  },
  NewNode.

handle_active_node(Node, NodeInfo) ->
  ActiveNodes = [{NodeInfo, erlang:system_time(millisecond)}],
  Node#node{active_nodes = ActiveNodes}.

handle_inactive_node(Node, _) ->
  Node#node{active_nodes = []}.

update_active_node(Node, NodeId) ->
  ActiveNodes = [{NodeId, erlang:system_time(millisecond)}],
  Node#node{active_nodes = ActiveNodes}.

send_message(Node, Message, TargetId) ->
  {_, _, NodePort} = dht_routing_table:get_node_info(Node#node.routing_table),
  TargetNodeInfo = dht_routing_table:get_node_info_by_id(Node#node.routing_table, TargetId),
  {_, Socket} = Node#node.socket,
  case TargetNodeInfo of
    {TargetIp, _} ->
      gen_udp:send(Socket, TargetIp, NodePort, term_to_binary(Message)),
      io:format("~nNodo trovato~n"),
      io:format("~nMessaggio ricevuto da ~p: ~p~n", [Node#node.node_id, Message]),
      io:format("~nRisposta: ~n"),
      {ok, {pong, {TargetId}}};
    {not_found} ->
      io:format("~nNodo non trovato~n"), % Gestisci l'errore di nodo di destinazione non trovato come desiderato
      {error, target_not_found}
  end.

listen(Node) ->
  receive
    {udp, _, _, _, BinMsg} ->
      Message = binary_to_term(BinMsg),
      handle_message(Node, Message)
  end,
  listen(Node).

handle_message(Node, {ping, SenderId}) ->
  Reply = {pong, {Node#node.node_id}},
  update_active_node(Node, SenderId),
  Reply;

handle_message(Node, {store, Key, Value}) ->
  dht_datastore:store(Node, Key, Value);

handle_message(Node, {find_node, TargetId}) ->
  Nodes = dht_routing_table:find_closest_nodes(Node, TargetId),
  Reply = {node_reply, Nodes},
  send_message(Node, Reply, TargetId);

handle_message(Node, {find_value, Key}) ->
  case dht_datastore:lookup(Node, Key) of
    {ok, Value} ->
      Reply = {value_reply, Key, Value},
      send_message(Node, Reply, Key);
    error ->
      Nodes = dht_routing_table:find_closest_nodes(Node, Key),
      Reply = {node_reply, Nodes},
      send_message(Node, Reply, Key)
  end;

handle_message(Node, {pong, TargetId}) ->
  update_active_node(Node, TargetId),
  ok.

print_node(Node) ->
  io:format("~n~nINFORMAZIONI NODO: ~n~n"),
  io:format("ID Nodo: ~p~n", [Node#node.node_id]),
  io:format("Info Nodo: ~p~n", [dht_routing_table:get_node_info(Node#node.routing_table)]),
  io:format("Routing Table: ~p~n", [dht_routing_table:get_all_nodes(Node#node.routing_table)]),
  io:format("Socket Nodo: ~p~n", [Node#node.socket]),
  io:format("Datastore: ~p~n", [Node#node.datastore]),
  io:format("Nodi attivi: ~p~n", [Node#node.active_nodes]).

ping(Node, TargetId) ->
  Message = {ping, Node#node.node_id},
  io:format("~nMessaggio: ~p~n", [Message]),
  PongMessage = send_message(Node, Message, TargetId),
  io:format("~nMessaggio di Pong: ~p~n", [PongMessage]),
  case PongMessage of
    {ok, {pong, {TargetId}}} ->
      % Risposta ricevuta con successo
      update_active_node(Node, TargetId),
      ok;
    {ok, _} ->
      {error, timeout};
    {error, Reason} ->
      {error, Reason}
  end.

stop(Node) ->
  {_, Socket} = Node#node.socket,
  gen_udp:close(Socket),
  NewRoutingTable = dht_routing_table:remove_node(Node#node.node_id, Node#node.routing_table),
  NewDatastore = dht_datastore:init(),
  NewNode = Node#node{
    socket = Socket,
    routing_table = NewRoutingTable,
    datastore = NewDatastore},
  NewNode.