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


app.ports.getExtraInfo.subscribe(function (userDay) {
  console.log(`Getting extra info for`, userDay)
  const userId = userDay[0]
  const day = userDay[1]
  client.getExtraInfo(userId, day)
    .then(result => {
      console.log(result);
      const timecards = result.data.User.timecards
      const timecard = timecards.length > 0 ? timecards[0] : null
      console.log(timecard)
      const extraInfo = {timecard}
      app.ports.receiveExtraInfo.send(extraInfo);
    })
    .catch(error => {
      console.log(error);
    });
});

app.ports.clockinExtra.subscribe(function (timecardClockin) {
  console.log(`Getting extra info for`, timecardClockin)
  const timecardId = timecardClockin[0]
  const clockinTs = timecardClockin[1]
  console.log(timecardId)
  console.log(clockinTs)
  console.log(client.clockinExtra)
  client.clockinExtra(timecardId, clockinTs)
    .then(result => {
      console.log("result!", result);
      const {clockinTs, clockoutTs, effectiveDt} = result.data.updateTimecard
      const extraInfo = {timecard: {clockinTs, clockoutTs, effectiveDt}}
      console.log(extraInfo)
      app.ports.receiveExtraInfo.send(extraInfo);
    })
    .catch(error => {
      console.log("ERROR!", error);
    });
});

app.ports.createExtraSchedule.subscribe(function (scheduleParams) {
  console.log(`Getting extra info for`, scheduleParams)
  const date = scheduleParams[0]
  const title = scheduleParams[1]
  const startTm = scheduleParams[2]
  client.createSchedule(date, title, startTm)
    .then(result => {
      console.log("result!", result);
      const timecard = result.data
      // app.ports.receiveExtraInfo.send(timecard);
    })
    .catch(error => {
      console.log(error);
    });
});
