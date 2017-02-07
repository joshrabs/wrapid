'use strict';

require('./main.css');
const Elm = require('./Main.elm');

import client from './gqlClient';

const app = Elm.Main.fullscreen();

// client.getAllProfiles();

app.ports.getAllProfiles.subscribe(function () {
  client.getAllProfiles()
    .then(result => {
      const frmt = result.allBaseProfiles.map(e => {
        const obj = {};
        obj.id = e.id;
        obj.firstName = e.firstName;
        if (e.file) {
          obj.url = e.file.url;
        } else {
          obj.url = null;
        }
        return obj;
      });
      console.log(frmt);
      app.ports.receiveNames.send(frmt);
    })
    .catch(error => {
      console.log(error);
    });
});
