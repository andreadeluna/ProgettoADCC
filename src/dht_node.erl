%% Modulo dht_node
-module(dht_node).

% Inclusione della definizione dei record
-include("dht_node.hrl").

% Importazione delle funzioni di supporto da altri moduli
-import(dht_routing_table,
[get_own_ip/0,
get_udp_port/0]
).

% Esportazione funzioni
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

%% Avvio e creazione del nodo
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

%% Generazione ID in modo casuale
generate_random_id() ->
  Bytes = crypto:strong_rand_bytes(20),
  Bytes.

%% Hash dell'ID generato casualmente
hash_id(Id) ->
  Hash = crypto:hash(sha, Id),
  erlang:phash2(Hash).

%% Connessione di due nodi alla stessa rete
join_network(NewNode, ExistingNode) ->
  ExistingNodeInfo = #node_info{
    id = ExistingNode#node.node_id,
    ip = element(1, dht_routing_table:get_node_info_by_id(ExistingNode#node.routing_table, ExistingNode#node.node_id)),
    port = element(2, dht_routing_table:get_node_info_by_id(ExistingNode#node.routing_table, ExistingNode#node.node_id))
  },
  NewNodeInfo = #node_info{
    id = NewNode#node.node_id,
    ip = element(1, dht_routing_table:get_node_info_by_id(ExistingNode#node.routing_table, ExistingNode#node.node_id)),
    port = element(2, dht_routing_table:get_node_info_by_id(ExistingNode#node.routing_table, ExistingNode#node.node_id))
  },

  %% Aggiornamento dei parametri dei nodi e popolamento della routing table e dei nodi attivi
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

%% Ricerca di un nodo all'interno della rete:
%% se non viene trovato viene mostrato il nodo più vicino all'ID cercato
lookup_node(Node, TargetId) ->
  case dht_routing_table:find_node(Node#node.routing_table, TargetId) of
    [] ->
      %% Se non ci sono nodi nella rete viene segnalato un errore
      io:format("~nNodo non trovato~n"),
      {error, not_found};
    [NodeInfo|_] ->
      %% Se ci sono nodi nella rete viene effettuata una ricerca
      io:format("~nNodo più vicino trovato~n"),
      lookup_node_recursive(Node, NodeInfo)
  end.

%% Ricerca ciclica in tutti i nodi della routing table
lookup_node_recursive(Node, NodeInfo) ->
  {_, NodeInfoID} = NodeInfo,
  %% Se il nodo viene raggiunto
  case ping(Node, NodeInfoID#node_info.id) of
    ok ->
      %% Gestione dei nodi attivi e visualizzazione informazioni del nodo
      handle_active_node(Node, NodeInfo),
      io:format("~nNodo raggiunto con successo~n"),
      {ok, NodeInfo};
    {error, timeout} ->
      %% Gestione dei nodi inattivi e ricerca ciclica
      handle_inactive_node(Node, NodeInfo),
      %% Se non ci sono nodi nella rete viene segnalato un errore
      case dht_routing_table:find_closest_nodes(Node#node.routing_table, NodeInfoID) of
        [] ->
          {error, not_found};
        %% Se ci sono altri nodi continua la ricerca
        [NextNode|_] ->
          lookup_node_recursive(Node, NextNode)
      end
  end.

%% Ricerca dei nodi più vicini all'ID cercato
find_node(Node, TargetId) ->
  %% Ricerca dei nodi più vicini all'ID cercato
  Nodes = dht_routing_table:find_closest_nodes(Node#node.routing_table, TargetId),
  case Nodes of
    [] ->
      % Se non viene trovato nessun nodo viene restituito un errore
      {error, node_not_found};
    [{_, ClosestNode} | _RestNodes] ->
      % Viene preso l'ID del nodo più vicino e viene usato come destinazione
      Reply = {node_reply, Nodes},
      io:format("~nID più vicino: ~p~n", [ClosestNode#node_info.id]),
      send_message(Node, Reply, ClosestNode#node_info.id)
  end.

%% Ricerca di un valore tramite chiave all'interno del datastore di un nodo
find_value(Node, Key) ->
  case dht_datastore:lookup(Node#node.datastore, Key) of
    {ok, Value} ->
      %% Se la chiave viene trovata viene restituito il valore associato
      Reply = {value_reply, Key, Value},
      send_message(Node, Reply, Node#node.node_id);
    error ->
      %% Se la chiave non viene trovata vengono mostrati i nodi più vicini all'ID cercato
      Nodes = dht_routing_table:find_closest_nodes(Node#node.routing_table, Node#node.node_id),
      case Nodes of
        [] ->
          % Se non viene trovato nessun nodo viene restituito un errore
          {error, node_not_found};
        [{_, ClosestNode} | _RestNodes] ->
          % Viene preso l'ID del nodo più vicino e viene usato come destinazione
          Reply = {node_reply, Nodes},
          io:format("~nID nodo più vicino: ~p~n", [ClosestNode#node_info.id]),
          send_message(Node, Reply, ClosestNode#node_info.id)
      end
  end.

%% Salvataggio di una coppia "chiave"-"valore" all'interno del datastore di un nodo
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

%% Gestione dei nodi attivi
handle_active_node(Node, NodeInfo) ->
  ActiveNodes = [{NodeInfo, erlang:system_time(millisecond)}],
  Node#node{active_nodes = ActiveNodes}.

%% Gestione dei nodi inattivi
handle_inactive_node(Node, _) ->
  Node#node{active_nodes = []}.

%% Aggiornamento dei nodi attivi
update_active_node(Node, NodeId) ->
  ActiveNodes = [{NodeId, erlang:system_time(millisecond)}],
  Node#node{active_nodes = ActiveNodes}.

%% Invio di un messaggio ad un nodo
send_message(Node, Message, TargetId) ->
  {_, _, NodePort} = dht_routing_table:get_node_info(Node#node.routing_table),
  TargetNodeInfo = dht_routing_table:get_node_info_by_id(Node#node.routing_table, TargetId),
  {_, Socket} = Node#node.socket,
  case TargetNodeInfo of
    %% Se il nodo viene trovato
    {TargetIp, _} ->
      %% Riceve il messaggio e risponde con un messaggio di pong
      gen_udp:send(Socket, TargetIp, NodePort, term_to_binary(Message)),
      io:format("~nNodo trovato~n"),
      io:format("~nMessaggio ricevuto da ~p: ~p~n", [Node#node.node_id, Message]),
      io:format("~nRisposta: ~n"),
      {ok, {pong, {TargetId}}};
    %% Se il nodo non viene trovato
    {not_found} ->
      %% Viene segnalato l'errore
      io:format("~nNodo non trovato~n"), % Gestisci l'errore di nodo di destinazione non trovato come desiderato
      {error, target_not_found}
  end.

%% Funzione di ascolto di un nodo durante il suo funzionamento
listen(Node) ->
  receive
    {udp, _, _, _, BinMsg} ->
      Message = binary_to_term(BinMsg),
      handle_message(Node, Message)
  end,
  listen(Node).

%% Gestione dei messaggi di ping
handle_message(Node, {ping, SenderId}) ->
  Reply = {pong, {Node#node.node_id}},
  update_active_node(Node, SenderId),
  Reply;

%% Gestione dei messaggi di store
handle_message(Node, {store, Key, Value}) ->
  dht_datastore:store(Node, Key, Value);

%% Gestione dei messaggi di ricerca di un nodo
handle_message(Node, {find_node, TargetId}) ->
  Nodes = dht_routing_table:find_closest_nodes(Node, TargetId),
  Reply = {node_reply, Nodes},
  send_message(Node, Reply, TargetId);

%% Gestione dei messaggi di ricerca di un valore
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

%% Gestione dei messaggi di pong
handle_message(Node, {pong, TargetId}) ->
  update_active_node(Node, TargetId),
  ok.

%% Stampa delle informazioni di un nodo
print_node(Node) ->
  io:format("~n~nINFORMAZIONI NODO: ~n~n"),
  io:format("ID Nodo: ~p~n", [Node#node.node_id]),
  io:format("Info Nodo: ~p~n", [dht_routing_table:get_node_info(Node#node.routing_table)]),
  io:format("Routing Table: ~p~n", [dht_routing_table:get_all_nodes(Node#node.routing_table)]),
  io:format("Socket Nodo: ~p~n", [Node#node.socket]),
  io:format("Datastore: ~p~n", [Node#node.datastore]),
  io:format("Nodi attivi: ~p~n", [Node#node.active_nodes]).

%% Invia un messaggio di ping ad un nodo per verificare che sia connesso e attivo
ping(Node, TargetId) ->
  Message = {ping, Node#node.node_id},
  io:format("~nMessaggio: ~p~n", [Message]),
  PongMessage = send_message(Node, Message, TargetId),
  io:format("~nMessaggio di Pong: ~p~n", [PongMessage]),
  case PongMessage of
    %% Risposta ricevuta con successo
    {ok, {pong, {TargetId}}} ->
      %% Aggiornamento nodi attivi
      update_active_node(Node, TargetId),
      ok;
    %% Risposta non ricevuta
    {ok, _} ->
      {error, timeout};
    %% Segnalazione errore
    {error, Reason} ->
      {error, Reason}
  end.

%% Stop di un nodo in esecuzione
stop(Node) ->
  {_, Socket} = Node#node.socket,
  %% Chiusura socket
  gen_udp:close(Socket),
  %% Rimozione nodi collegati dalla routing table
  NewRoutingTable = dht_routing_table:remove_node(Node#node.node_id, Node#node.routing_table),
  %% Eliminazione valori dal datastore
  NewDatastore = dht_datastore:init(),
  %% Aggiornamento del record
  NewNode = Node#node{
    socket = undefined,
    routing_table = NewRoutingTable,
    datastore = NewDatastore,
    active_nodes = []
  },
  io:format("~nNodo stoppato con successo~n"),
  NewNode.