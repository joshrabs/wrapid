'use strict';

require('./main.css');
const Elm = require('./Main.elm');

import client from './gqlClient';

const app = Elm.Main.fullscreen();
console.log(app)

// client.getAllProfiles();

// app.ports.getAllProfiles.subscribe(function () {
//   client.getAllProfiles()
//     .then(result => {
//       console.log(result);
//       const frmt = result.data.allBaseProfiles.map(e => {
//         let obj = {
//           id: e.id,
//           firstName: e.firstName
//         };
//
//         obj.url = e.file ? e.file.url : null;
//
//         return obj;
//       });
//       console.log(frmt);
//       app.ports.receiveNames.send(frmt);
//     })
//     .catch(error => {
//       console.log(error);
//     });
// });


app.ports.getExtraInfo.subscribe(function (userDay) {
  console.log(`Getting extra info for`, userDay)
  const userId = userDay[0]
  const day = userDay[1]
  console.log(userId)
  console.log(day)
  client.getExtraInfo(userId, day)
    .then(result => {
      console.log(result);
      const {baseprofile, timecards, extraschedule} = result.data.User
      console.log(baseprofile)
      console.log("timecards!", timecards)
      const timecard = timecards.length > 0 ? timecards[0] : null
      console.log(timecard)
      const scheduleItems = extraschedule.extrascheduleitemses
      const schedule = {id: extraschedule.id, items: scheduleItems}
      const profile = baseprofile
      const extraInfo = {timecard, profile, schedule}
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
      const {id, clockinTs, clockoutTs, effectiveDt} = result.data.updateTimecard
      const timecard = {id, clockinTs, clockoutTs, effectiveDt}
      app.ports.receiveTimecardUpdate.send(timecard);
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


app.ports.getAllExtraInfo.subscribe(function(params) {
  const d = params[0]
  console.log(d);
  client.getAllExtraInfo(d)
    .then(result => {
      const allUsers = result.data.allUsers
      console.log(result.data.allUsers)
      const mapInfo = function(extra) {
        console.log(extra)
        const {id, baseprofile} = extra;
        const {firstName, lastName, avatar} = baseprofile
        const avatarSrc = avatar ? avatar.url : null
        return {id, firstName, lastName, avatarSrc}
      }
      const extraInfo = allUsers.map(mapInfo)
      console.log(extraInfo)
      app.ports.receiveAllExtraInfo.send(extraInfo);
    })
    .catch(error => {
      console.log(error);
    });
})


app.ports.subExtraSchedule.subscribe(function() {
  console.log(client.subExtraSchedule())
  client.subExtraSchedule()
    .latest()
    .then(result => {
      console.log(result);
      const schedule = result.User.data;
      app.ports.subReceiveExtraSchedule.send(schedule)
    })
    .catch(error => {
      console.log(error);
    })
})


app.ports.addScheduleItem.subscribe(function(extraId, scheduleItem) {
  client.addScheduleItem()
    .then(result => {
      console.log(result);
    })
    .catch(error =>{
      console.log(error)
    })
})
