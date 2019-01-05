import './main.css';
import { Elm } from './Main.elm';
import registerServiceWorker from './registerServiceWorker';
import PouchDB from 'pouchdb';

var db = new PouchDB('sticky-stories');

var app = Elm.Main.init({
  node: document.getElementById('root')
});
app.ports.storePort.subscribe(function(data) {
  console.log("Port out: ", data);
});

// db.changes().on('change', function(data) {
//   console.log("Port in: ", data);
//   //app.ports.activeUsers.send(data);
// });

registerServiceWorker();
