'use strict';

require('./main.css');
const Elm = require('./Main.elm');

import client from './gqlClient';

const app = Elm.Main.fullscreen();
console.log(app)

// client.getAllProfiles();

app.ports.getAllProfiles.subscribe(function () {
  client.getAllProfiles()
    .then(result => {
      console.log(result);
      const frmt = result.data.allBaseProfiles.map(e => {
        let obj = {
          id: e.id,
          firstName: e.firstName
        };

        obj.url = e.file ? e.file.url : null;

        return obj;
      });
      console.log(frmt);
      app.ports.receiveNames.send(frmt);
    })
    .catch(error => {
      console.log(error);
    });
});
