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
