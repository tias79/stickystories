import './main.css';
import { Elm } from './Main.elm';
import registerServiceWorker from './registerServiceWorker';
import PouchDB from 'pouchdb';

var db = new PouchDB('sticky-stories');

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
app.ports.storePort.subscribe(function(data) {
  let json = data.obj;
  switch (data.type) {
    case "US":
      json._id = "US-" + json.id;
      break;
    case "Task":
      json._id = "Task-" + json.id;
      break;
  }
  //db.put(json);

  console.log(data);
});

// db.changes().on('change', function(data) {
//   console.log("Port in: ", data);
//   //app.ports.activeUsers.send(data);
// });

registerServiceWorker();
