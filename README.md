# ProgettoADCC
## Distributed Hash Table (DHT)
Una Distributed Hash Table (DHT) è una soluzione scalabile che richiede una quantità di memoria limitata. 
In particolare la memoria richiesta da ciascun nodo dipende principalmente dal numero di chiavi o dati che quel nodo gestisce e dalla specifica implementazione della DHT.

Una DHT è una rete auto-organizzata in cui i nodi si organizzano autonomamente senza un'autorità centrale. 
I nodi entrano e lasciano il sistema in modo dinamico, e la DHT si adatta automaticamente a questi cambiamenti. 
Tuttavia, la DHT richiede un meccanismo di routing per individuare le risorse all'interno del sistema distrito che può coinvolgere la comunicazione tra nodi per trovare le informazioni desiderate.

Nello specifico, una Distributed Hash Table è una tabella hash distribuita in cui le voci (entry) sono distribuite tra i nodi del sistema. 
I nodi si passano la tabella hash o le informazioni necessarie per gestire la tabella. Il meccanismo di routing consente ai nodi di instradare le richieste verso il nodo appropriato che gestisce la risorsa. Questo viene fatto utilizzando gli ID assegnati ai nodi e ai dati. 
Tuttavia, la distribuzione delle porzioni di tabella hash tra i nodi può variare a seconda dell'algoritmo di routing utilizzato nella DHT.

Ad ogni nodo viene assegnato un ID che fa parte di un determinato range di valori. Allo stesso modo, i dati che i nodi mantengono in memoria vengono associati a un ID che rientra nello stesso intervallo di valori. Questo schema di assegnazione degli ID consente di instradare le richieste in base alla loro posizione nel range di valori.
Per ridurre il numero di salti necessari per il routing e creare un effetto "small world", i nodi possono mantenere collegamenti con altri nodi che sono a varie distanze. Questo consente ai nodi di raggiungere nodi lontani nel sistema senza dover attraversare un gran numero di salti intermedi. Questo tipo di struttura dei collegamenti può migliorare l'efficienza del routing nella DHT.

Riassumento, una Distributed Hash Table è un sistema distribuito che consente di archiviare e recuperare dati in modo efficiente su una rete peer-to-peer (P2P). 
Sfrutta l'idea di una tabella hash distribuita, in cui i dati sono suddivisi in una serie di coppie chiave-valore. Ogni nodo nella rete P2P gestisce una porzione di questa tabella hash distribuita. Utilizzando algoritmi di routing specifici, i nodi sono in grado di cercare, inserire e recuperare le coppie chiave-valore all'interno della DHT in modo efficiente e scalabile.

## Kademlia
Kademlia è un protocollo di rete e un algoritmo di routing utilizzato in molte implementazioni di DHT, noto per la sua scalabilità, la resilienza alle disconnessioni dei nodi e l'efficienza nelle operazioni di ricerca e archiviazione.
Il funzionamento di Kademlia si basa su diverse caratteristiche chiave:
1. Identificatori e distanze: Ogni nodo e ogni coppia chiave-valore in Kademlia è identificata da un ID, che è una stringa binaria di lunghezza fissa. Le distanze tra gli ID vengono calcolate utilizzando l'operazione XOR.
2. Tabelle di routing: Ogni nodo in Kademlia mantiene una tabella di routing che contiene informazioni sui nodi vicini nella rete. La tabella di routing è organizzata in bucket, e ogni bucket rappresenta una porzione dell'intervallo degli ID possibili. Ogni bucket contiene una lista di nodi che sono responsabili per quella porzione dell'intervallo.
3. K-buckets: I k-buckets sono i contenitori principali all'interno delle tabelle di routing. Ogni k-bucket rappresenta un intervallo di ID e contiene una lista di nodi che sono vicini a quell'intervallo. I k-buckets sono mantenuti in ordine rispetto all'ultima attività dei nodi, in modo che i nodi più recentemente attivi siano in fondo alla lista.
4. Operazioni di routing: Per cercare, inserire o recuperare dati nella DHT, i nodi utilizzano operazioni di routing. Durante un'operazione di routing, un nodo seleziona i nodi più vicini all'ID di destinazione e invia loro richieste per ottenere informazioni sulla posizione del nodo responsabile o per archiviare il dato.
5. Scambio di informazioni: Durante le operazioni di routing, i nodi scambiano informazioni tra loro per aggiornare le proprie tabelle di routing. Ciò consente ai nodi di apprendere e memorizzare informazioni sui nodi vicini e sulle loro responsabilità.
6. Copia ridondante: Kademlia implementa una strategia di ridondanza per aumentare l'affidabilità dei dati archiviati nella DHT. Ogni coppia chiave-valore viene archiviata su più nodi, in modo che se un nodo si disconnette o fallisce, altri nodi nella rete possano ancora fornire il dato richiesto.

Ogni nodo in Kademlia ha molti contatti con nodi che hanno prefissi simili al proprio. Man mano che ci si allontana dal nodo, i collegamenti diventano più ampi. Ciò significa che durante la generazione delle tabelle di routing, ogni nodo avrà un solo collegamento con nodi che non hanno alcun prefisso in comune, e man mano che il prefisso aumenta, aumentano anche i collegamenti.

Kademlia tiene traccia di una lista di nodi per ogni prefisso anziché un singolo nodo, allo scopo di avere ridondanza. Questo significa che per ogni prefisso, viene mantenuta una lista di nodi che possono gestire le richieste associate a quel prefisso.
Nel routing di Kademlia viene utilizzato un approccio iterativo. Quando un nodo deve effettuare una ricerca, viene inviata una richiesta a un insieme di nodi contemporaneamente, noto come α-nodes. Questi nodi rispondono con informazioni sui k nodi più vicini alla chiave cercata, che vengono successivamente interrogati. Questo processo iterativo continua finché non viene raggiunto il nodo responsabile della chiave o finché non si raggiunge un punto di stallo.

L'operatore XOR viene utilizzato in Kademlia per calcolare le distanze tra gli ID dei nodi e le chiavi. Quando una risorsa viene assegnata a un nodo, viene effettuata un'operazione XOR tra il valore della chiave e l'ID del nodo. Il nodo con il risultato più piccolo sarà responsabile della risorsa. Viene utilizzato lo XOR perché è una metrica di distanza e riflette il fatto che i nodi vicini condividono sotto-alberi simili, con una distanza XOR ridotta.

La struttura delle tabelle di routing di Kademlia è organizzata in bucket. Ogni bucket contiene una serie di nodi che fanno parte di sotto-alberi specifici. I collegamenti nei bucket diventano più fitti man mano che ci si avvicina al nodo, consentendo una maggiore probabilità di trovare nodi vicini durante le query.
Il meccanismo di gestione dei bucket di Kademlia prevede diverse operazioni. Quando un nodo riceve un messaggio da un altro nodo, controlla se il nodo mittente è presente nel bucket. Se il nodo mittente non è presente, viene inserito se c'è spazio disponibile nel bucket. Se il bucket è pieno, viene inviato un messaggio di ping al nodo meno recente del bucket. Se il ping non riceve risposta entro un certo tempo, il nodo meno recente viene rimosso e sostituito dal nodo mittente.

Le tipologie di messaggi utilizzate in Kademlia sono PING (per verificare la disponibilità di un nodo), STORE (per memorizzare una nuova chiave), FIND_NODE (per trovare un nodo responsabile di una determinata chiave), FIND_VALUE (per recuperare il valore di una chiave) e STORE (per inserire una coppia chiave-valore nel nodo responsabile).
Quando un nuovo nodo si unisce a Kademlia, effettua una ricerca tramite il nodo di bootstrap per avvicinarsi ai nodi simili. Il nodo di bootstrap fornisce una lista di nodi che il nuovo nodo interrogherà per riempire i suoi bucket. In questo modo, il nuovo nodo diventa noto agli altri nodi e viene incorporato nelle loro tabelle di routing.
Quando un nodo lascia Kademlia, non esiste una procedura specifica da seguire. Tuttavia, quando gli altri nodi cercano di contattare il nodo che è uscito, non riceveranno risposta e nel tempo il nodo uscito verrà eliminato dalle tabelle di routing degli altri nodi.


In sintesi, Kademlia è un algoritmo di routing e un protocollo per le DHT che consente ai nodi di cercare, archiviare e recuperare dati in modo efficiente e scalabile all'interno di una rete P2P. Utilizzando identificatori, distanze, tabelle di routing e operazioni di routing, Kademlia offre una soluzione decentralizzata per la gestione dei dati su una rete distribuita.

## Erlang
Erlang è un linguaggio di programmazione general-purpose, concorrente e ad alte prestazioni, sviluppato presso Ericsson nel 1986. 
È noto per il suo supporto nativo per la concorrenza, l'isolamento dei processi e la gestione degli errori. Erlang è ampiamente utilizzato per lo sviluppo di sistemi distribuiti, particolarmente adatto per applicazioni di telecomunicazione e sistemi P2P.
Con esso è possibile implementare un sistema Kademlia in quanto fornisce gli strumenti e le caratteristiche necessarie per la creazione di sistemi distribuiti e concorrenti, che sono fondamentali per implementare una DHT come Kademlia.
Con Erlang, è possibile creare nodi che comunicano tra loro utilizzando il modello di scambio di messaggi asincrono. Questo è un aspetto cruciale per il funzionamento di un sistema Kademlia, in cui i nodi si scambiano informazioni e richieste di routing.
Inoltre, Erlang offre meccanismi di gestione degli errori e tolleranza ai guasti che sono essenziali per garantire la resilienza e la stabilità di un sistema distribuito. Questi meccanismi consentono di gestire disconnessioni di nodi, errori di rete e altre situazioni impreviste che possono verificarsi in un ambiente distribuito.
Complessivamente, grazie alle sue caratteristiche di concorrenza, scalabilità e tolleranza ai guasti, Erlang è una scelta appropriata per implementare un sistema Kademlia o qualsiasi altro sistema distribuito simile.

### dht_datastore.hrl
File di intestazione (header file) utilizzato per definire il record di dati chiamato "dht_datastore". 
Un record in Erlang è una struttura dati che può contenere campi con valori associati.

Nel file dht_datastore.hrl, il record "dht_datastore" viene definito con un singolo campo chiamato "data". Questo record viene utilizzato nel modulo dht_datastore.erl per rappresentare lo stato del datastore distribuito utilizzato nel sistema DHT (Distributed Hash Table).

Il campo "data" nel record dht_datastore rappresenta il contenuto effettivo del datastore, che viene implementato come un dizionario Erlang. 
I dati nel dizionario sono memorizzati utilizzando una chiave (Key) associata a un valore (Value). 
Il modulo dht_datastore.erl utilizza le funzioni del modulo dict per gestire l'aggiunta, la ricerca e l'accesso ai dati nel datastore.

In sintesi, il file dht_datastore.hrl definisce la struttura dei dati del datastore utilizzato e viene incluso nel modulo dht_datastore.erl per consentire l'utilizzo del record "dht_datastore" e del campo "data" all'interno del codice.
