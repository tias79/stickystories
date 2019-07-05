import './main.css';
import { Elm } from './Main.elm';
import registerServiceWorker from './registerServiceWorker';
import PouchDB from 'pouchdb';
import PouchDBUpsert from 'pouchdb-upsert';

PouchDB.plugin(PouchDBUpsert);
//var db = new PouchDB('sticky-stories');

var remoteDB = new PouchDB("http://localhost:5984/mydb", {
  live: true,
  retry: true
});

//var localDB = new PouchDB("mydb").sync(remoteDB).plugin(PouchDBUpsert);
//var localDB = PouchDB.sync("mydb", "http://localhost:5984/mydb");


var localDB = new PouchDB("mydb");
localDB.replicate.to(remoteDB, {
  live: true,
  retry: true
});
localDB.replicate.from(remoteDB, {
  live: true,
  retry: true
});


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
app.ports.sendPort.subscribe(function(data) {
  console.log("Port out: ", data);
  return;
  let json = data.obj;
  switch (data.type) {
    case "US":
      json._id = "US-" + json.id;
      break;
    case "Task":
      json._id = "Task-" + json.id;
      break;
  }
  
  localDB.upsert(json._id, (doc) => Object.assign(doc, json)).then((doc) => localDB.get(json._id).then((doc) => console.log("doc: ", doc)));

//  db.get(json._id).then((doc) => console.log("doc: ", doc));
  console.log(data);
});

localDB.changes({
  since: 'now',
  live: true,
  include_docs: true
}).on('change', function(change) {
  console.log("Port in: ", change);
  
  if (change.id.startsWith("US")) {
    //app.ports.receivePort.send({ "type": "US", "obj": change.doc});
  } else if (change.id.startsWith("Task")) {
    //app.ports.receivePort.send({ "type": "Task", "obj": change.doc});
  } else {
    console.error("Unknown type: " + change.id);
  }
}).on('error', function (err) {
  console.error(err);
});

registerServiceWorker();
