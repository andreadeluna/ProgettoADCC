# Distributed Hash Table
![Erlang](https://img.shields.io/badge/Erlang-red.svg?style=flat-square&logo=erlang)

## Progetto di Applicazioni Distribuite e Cloud Computing

### Appello:
Secondo appello, Sessione Estiva 2022/2023

### Studente:
* [Andrea De Luna](https://github.com/andreadeluna), **Matricola :** 313076

* [Veronica Michelini](https://github.com/VeronicaMichelini), **Matricola :** 317197

-----------------------------------------------------
## Obiettivo
Il progetto consiste nell'implementazione di una Distributed Hash Table (DHT), il cui funzionamento si basa sul protocollo Kademlia.


## Preparazione ambiente

#### Primo avvio
Per procedere con il primo avvio, è necessario posizionarsi all'interno della cartella src, contenente i moduli atti al funzionamento del progetto.
In seguito, è necessario compilare il codice, che non deve mostrare errori di compilazione:

```
erlc *.erl
```

Oppure, allo stesso modo, è possibile aprire la shell di Erlang con il comando

```
erl -sname nome_host@localhost
```

e in seguito compilare i moduli al proprio interno uno alla volta, con i comandi

```
c(dht_routing_table),
c(dht_datastore),
c(dht_utils),
c(dht_node).
```
## Funzionamento classico
#### Creazione nodi
È necessario, successivamente, avviare i nodi e connetterli alla rete. È possibile effettuare
l'operazione in modo manuale con i comandi:

```
Node1 = dht_node:start(),
Node2 = dht_node:start(),
Node3 = dht_node:start(),
Node4 = dht_node:start(),
...

{Connected1, Connected2} = dht_node:joinnetwork(Node1, Node2),
{Connected3, Connected4} = dht_node:joinnetwork(Node3, Node4),
...
```

È possibile tuttavia eseguire il processo anche in modo automatizzato, indicando il numero di nodi desiderati:

```
Nodes = dht_node:create_nodes(4),

Network = dht_node:connect_nodes(Nodes),

Connected = lists:nth(4, Network).
```

In questo modo verrà preso in considerazione l'ultimo nodo connesso, comprensivo di routing table aggiornata e contenente la lista degli altri nodi collegati.

#### Ping
Per verificare se un nodo è connesso all'interno della rete è sufficiente eseguire il comando

```
dht_node:ping(Connected, Id).
```

che restituirà un messaggio di pong, nel caso in cui il nodo sia presente, oppure di errore nel caso in cui il nodo non esista oppure non sia connesso all'interno della rete.

#### Store
Per salvare una coppia "chiave"-"valore" all'interno del datastore di un nodo è sufficiente eseguire il comando

```
NewConnected = dht_node:store(Connected, "chiave", "valore").
```

che permetterà di aggiornare il datastore del nodo indicato assegnandolo ad una nuova variabile, inserendovi all'interno i parametri passati.
Nel caso in cui il nodo non venga trovato verrà mostrato un messaggio di errore.

#### Find value
Per cercare un valore tramite chiave all'interno di un nodo sarà sufficiente eseguire il comando

```
dht_node:find_value(NewConnected, "chiave").
```

che restituirà il relativo valore, se trovato all'interno del datastore del nodo indicato.
Nel caso in cui il valore non venga trovato verrà restituito un messaggio indicante i tre nodi più vicini al
nodo indicato al momento della chiamata della funzione, in modo da poter cercare anche in essi il valore desiderato.

#### Find node
Per cercare i nodi più vicini ad un nodo indicato è sufficiente eseguire il comando

```
dht_node:find_node(NewConnected, Id).
```

che restituirà per l'appunto una lista contenente le informazioni dei tre nodi più vicini all'ID indicato.
Nel caso in cui il nodo sia presente verrà restituito anche un messaggio di pong da parte del nodo trovato,
nel caso in cui invece il nodo non sia presente verrà contattato il nodo più vicino all'ID desiderato,
che risponderà anch'esso con un messaggio di pong.

#### Lookup
Per cercare un nodo specifico all'interno della rete è sufficiente eseguire il comando

```
dht_node:lookup_node(NewConnected, Id).
```

che restituirà, se il nodo è stato trovato, un messaggio di pong da parte di esso, contenente le relative informazioni del nodo.
Se invece il nodo non è stato trovato, verrà ricevuto invece un messaggio di pong da parte del nodo più vicino all'ID indicato
in fase di chiamata della funzione.

#### Send message
Per inviare un messaggio da un nodo ad un altro all'interno della rete è sufficiente eseguire il comando

```
dht_node:send_message(NewConnected, Message, Id).
```

che restituirà, se il nodo è stato trovato, un messaggio di pong con conseguente visualizzazione del messaggio inviato e del destinatario.
Nel caso in cui il nodo non venga trovato, verrà visualizzato un messaggio di errore.

#### Print node

Per stampare le informazioni relative ad un nodo è sufficiente eseguire il comando

```
dht_node:print_node(NewConnected).
```

#### Stop
Per stoppare l'esecuzione di un nodo è sufficiente eseguire il comando

```
StopNewConnected = dht_node:stop(NewConnected).
```

che ne fermerà l'esecuzione e assegnerà il nuovo valore del nodo ad una nuova variabile,
contenente i parametri aggiornati, quali la routing table senza nodi connessi, la lista vuota di nodi attivi
e una socket indefinita.


## Funzionamento con Message Passing

Il funzionamento del programma tramite message passing è analogo a quello illustrato nella sezione precedente.
Tuttavia, è necessario eseguire alcuni passaggi aggiuntivi. A seguito dell'esecuzione sarà
possibile visionare la response del messaggio inviato tramite il comando flush().

#### Attivazione gestore nodo

A seguito della creazione del nodo, sarà necessario attivare un gestore, che permetterà di gestirne i relativi
messaggi.

```
Pid = dht_node:start_handler(Connected).
```

#### Visualizzazione dati nodo

Per visualizzare i dati contenuti all'interno di un nodo è sufficiente eseguire il comando

```
Pid ! {get_node_info, self()}.
```

#### Ping

Per effettuare l'operazione di ping è sufficiente eseguire il comando

```
Pid ! {ping, Id, self()}.
```

#### Store

Per effettuare l'operazione di store è sufficiente eseguire il comando

```
Pid ! {store, Key, Value, self()}.
```

#### Find value

Per effettuare l'operazione di find value è sufficiente eseguire il comando

```
Pid ! {find_value, Key, self()}.
```

#### Find node

Per effettuare l'operazione di find node è sufficiente eseguire il comando

```
Pid ! {find_node, Id, self()}.
```

#### Lookup

Per effettuare l'operazione di lookup è sufficiente eseguire il comando

```
Pid ! {lookup, Id, self()}.
```

#### Send message

Per effettuare l'operazione di send message è sufficiente eseguire il comando

```
Pid ! {send_message, Message, Id, self()}.
```

#### Stop

Per effettuare l'operazione di stop è sufficiente eseguire il comando

```
Pid ! {stop, self()}.
```

#### Invio nodo a shell remota

Per effettuare inviare il valore di un nodo ad una shell remota è necessario, per prima cosa,
connettere le shell con il comando

```
net_adm:ping(nome_host_remoto@localhost).
```

In seguito è necessario registrare il processo collegato al gestore del nodo in ascolto

```
register(my_node, Pid).
```

Infine, per inviare il valore, è sufficiente eseguire il comando

```
{my_node, 'nome_host_remoto@localhost'} ! Node.
```

#### Esecuzione comandi remota

Per eseguire dei comandi remoti da una shell all'interno di una seconda shell, è possibile utilizzare il comando

```
rpc:call('nome_host_remoto@localhost', nome_modulo, funzione, [parametri]).
```
