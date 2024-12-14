# ADCC-Kademlia

<a name="readme-top"></a>

 Kania, Nicholas  - <n.kania@campus.uniurb.it> <br>
 Leopizzi, Matteo  - <m.leopizzi1@campus.uniurb.it> <br>
 Pierucci, Giada  - <g.pierucci4@campus.uniurb.it>

<!-- TABELLA DEI CONTENUTI -->
<details>
  <summary>Tabella dei contenuti</summary>
  <ol>
    <li>
      <a href="#descrizione-del-progetto">Descrizione del progetto</a>
    </li>
    <li>
      <a href="#tecnologia">Tecnologia</a>
      <ul>
        <li><a href="#dependances">Dependances</a></li>
        <li><a href="#scelte-implementative">Scelte implementative</a></li>
      </ul>
    </li>
    <li>
      <a href="#stress-test"> StressTest </a>
    </li>
  </ol>
</details>

<!-- DESCRIZIONE DEL PROGETTO -->
## Descrizione del progetto

Il progetto si propone di implementare Kademlia in Erlang. 
Kademlia è un protocollo per tabelle hash distribuite per reti peer-to-peer decentralizzate....


<!-- MODULI -->
### Moduli

<!-- FUNZIONI -->
### Funzioni 
* `ping(Id)`: verifica l'attività di un node all'interno della rete. 
* `store(Key, Value)`: memorizza la coppia chiave-valore in un nodo. 
Funzionamento: il nodo che utilizza questa funzione crea una chiave (Key), la quale viene generata tramite una funzione di Hash ed, in seguito, ne memorizzerà anche il valore (Value) corrispondente. 
* `find_node(Id)`: il destinatoria restituirà i nodi più vicini all'Id indicato presenti all'interno dei suoi k-buckets.  
* `find_value(Key)`: trova il valore associato alla chiave indicata. 
Il destinatario del messaggio verifica la presenza o meno della chiave all'interno dei suoi buckets ed in caso affermativo restituisce il valore corrispondente; altrimenti inoltra la richiesta per riscontrare la chiave al nodo più vicino basandosi sulla distanza XOR. 
* `join(Id)`: funzione affinché un nodo posso unirsi alla rete, la quale presenta due scenari possibili:
    * qualora non ci fossero presenti ancora nodi all'interno della rete, allora il nodo in esame assume il ruolo di **bootstrap node**. 
    * altrimenti si ricorre all'utilizzo di PID/ID già esistenti all'interno della rete. 

Altre funzionalità: 
* calcolo ID Hash SHA-1
* calcolo distanza XOR 




<!-- TECNOLOGIA -->
## Tecnologia
<!-- DEPENDANCES -->
### Dependances

```erl
% codice
```