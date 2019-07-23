import './main.css';
import { Elm } from './Main.elm';
import registerServiceWorker from './registerServiceWorker';
import PouchDB from 'pouchdb';
import PouchDBUpsert from 'pouchdb-upsert';

const dbName = "test2";

PouchDB.plugin(PouchDBUpsert);

var remoteDB = new PouchDB("http://localhost:5984/" + dbName, {
  live: true,
  retry: true
});

var localDB = new PouchDB(dbName);
localDB.replicate.to(remoteDB, {
  live: true,
  retry: true
});
localDB.replicate.from(remoteDB, {
  live: true,
  retry: true
});

localDB.allDocs().then(doc => app.ports.receivePort.send(doc));

const crypto = window.crypto || window.msCrypto;
const getRandomInts = (n) => {
    const randInts = new Uint32Array(n);
    crypto.getRandomValues(randInts);
    return Array.from(randInts);
};
const randInts = getRandomInts(2);

var app = Elm.Main.init({
  node: document.getElementById('root'),
  flags: randInts
});
app.ports.sendPort.subscribe(function(json) {
  json._id = json.obj.id;
  console.log("Port out: ", json);
  
  localDB.upsert(json._id, (doc) => Object.assign(doc, json));
});

localDB.changes({
  since: 'now',
  live: true,
  include_docs: true
}).on('change', function(change) {
  console.log("Port in: ", change);
 
  app.ports.receivePort.send(change.doc);
}).on('error', function (err) {
  console.error(err);
});

registerServiceWorker();
