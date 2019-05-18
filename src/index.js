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
  console.log("Port out: ", data);
});

// db.changes().on('change', function(data) {
//   console.log("Port in: ", data);
//   //app.ports.activeUsers.send(data);
// });

registerServiceWorker();
