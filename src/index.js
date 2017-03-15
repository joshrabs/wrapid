'use strict';

require('./main.css');
const Elm = require('./Main.elm');

import client from './gqlClient';

const app = Elm.Main.fullscreen();
console.log(app);

app.ports.getExtraInfo.subscribe(function (userDay) {
  console.log(`Getting extra info for`, userDay);
  const userId = userDay[0];
  const day = userDay[1];
  console.log(userId);
  console.log(day);
  client.getExtraInfo(userId, day)
    .then(result => {
      console.log(result);
      const {baseprofile, timecards, extraschedule} = result.data.User;
      console.log(baseprofile);
      console.log('timecards!', timecards);
      const timecard = timecards.length > 0 ? timecards[0] : null;
      console.log(timecard);
      const scheduleItems = extraschedule.extrascheduleitemses;
      const schedule = {id: extraschedule.id, items: scheduleItems};
      const profile = baseprofile;
      const extraInfo = {extraId: userId, timecard, profile, schedule};
      app.ports.receiveExtraInfo.send(extraInfo);
    })
    .catch(error => {
      console.log(error);
    });
});

app.ports.clockinExtra.subscribe(function (timecardClockin) {
  console.log(`Getting extra info for`, timecardClockin);
  const timecardId = timecardClockin[0];
  const clockinTs = timecardClockin[1];
  console.log(timecardId);
  console.log(clockinTs);
  console.log(client.clockinExtra);
  client.clockinExtra(timecardId, clockinTs)
    .then(result => {
      console.log('result!', result);
      const {id, clockinTs, clockoutTs, effectiveDt} = result.data.updateTimecard;
      const timecard = {id, clockinTs, clockoutTs, effectiveDt};
      app.ports.receiveTimecardUpdate.send(timecard);
    })
    .catch(error => {
      console.log('ERROR!', error);
    });
});

app.ports.createExtraSchedule.subscribe(function (scheduleParams) {
  console.log(`Getting extra info for`, scheduleParams);
  const date = scheduleParams[0];
  const title = scheduleParams[1];
  const startTm = scheduleParams[2];
  client.createSchedule(date, title, startTm)
    .then(result => {
      console.log('result!', result);
      const timecard = result.data;
      // app.ports.receiveExtraInfo.send(timecard);
    })
    .catch(error => {
      console.log(error);
    });
});

app.ports.getAllExtraInfo.subscribe(function (params) {
  const d = params[0];
  console.log(d);
  client.getAllExtraInfo(d)
    .then(result => {
      let allUsers = result.data.allUsers;
      console.log(result.data.allUsers);
      let mapInfo = function (extra) {
        console.log(extra);
        let {id, baseprofile, extraschedule, timecards} = extra;
        let {firstName, lastName, avatar} = baseprofile;
        let avatarSrc = avatar ? avatar.url : null;
        let profile = {firstName, lastName, avatar};
        let schedule;
        if (extraschedule) {
          let eItems = extraschedule.extrascheduleitemses.map(function (it) {
            console.log(it);
            const {id, startTm, endTm, category, name} = it;
            return it;
          });
          schedule = {id: extraschedule.id, items: eItems};
        } else {
          schedule = {id: 'meow', items: [{startTm: '08:00', endTm: '10:00pm', category: 'Other', name: 'Call Start'}]};
        }

        console.log(timecards);

        let dtc = timecards.map(function (t) {
          const {clockinTs, clockoutTs, effectiveDt, id} = t;
          return {clockinTs, clockoutTs, effectiveDt, id};
        });

        const defTimecard = dtc[0] || {clockinTs: '08:00am', clockoutTs: null, effectiveDt: '2017-03-03', id: 'meow'};

        console.log(schedule);

        let frmt = {extraId: id, profile, schedule, timecard: defTimecard};
        console.log(frmt);
        return frmt;
      };
      const extraInfo = allUsers.map(mapInfo);
      console.log(extraInfo);
      app.ports.receiveAllExtraInfo.send(extraInfo);
    })
    .catch(error => {
      console.log(error);
    });
});

app.ports.fetchDailySkin.subscribe(function (date) {
  console.log(date);
  client.fetchDailySkin(date)
    .then(result => {
      console.log(result.data);
      const {data} = result;
      const skinResult = data.allSkins.length > 0 ? data.allSkins[0] : null;
      if(!skinResult){
        app.ports.receiveDailySkin.send(null);
        return;
      }
      const {skinItems} = skinResult;
      const frmtSkinItems = skinItems.map(function (item) {
        console.log(item);
        const {user, part, pay, callStartTs} = item;
        const {firstName, lastName, avatar} = user.baseprofile;
        let sAvatar = avatar;
        if (!avatar) {
          sAvatar = {url: null};
        }
        return {userId: user.id, firstName, lastName, part, pay, avatar: sAvatar, callStart: callStartTs};
      });
      console.log(frmtSkinItems);
      const skin = {effectiveDt: date, skinItems: frmtSkinItems};
      console.log(skin)
      app.ports.receiveDailySkin.send(skin);
    })
    .catch(error => {
      console.error(error);
    });
});

app.ports.subExtraSchedule.subscribe(function () {
  console.log(client.subExtraSchedule());
  client.subExtraSchedule()
    .latest()
    .then(result => {
      console.log(result);
      const schedule = result.User.data;
      app.ports.subReceiveExtraSchedule.send(schedule);
    })
    .catch(error => {
      console.log(error);
    });
});

app.ports.addScheduleItem.subscribe(function (params) {
  const scheduleItem = params[1];
  client.addScheduleItem(scheduleItem)
    .then(result => {
      console.log(result);
    })
    .catch(error => {
      console.log(error);
    });
});

app.ports.uploadSkin.subscribe(function(params){
  console.log(params)
  let {effectiveDt, skinItems} = params
  console.log(effectiveDt)
  console.log(skinItems)
  skinItems = skinItems.map(function(si) {
    return {
      "callStartTs": si.callStart
      , "callEndTs": ""
      , pay: si.pay
      , part: si.part
      , email: si.email
      , firstName: si.firstName
      , lastName: si.lastName
    }
  })
  console.log(skinItems)
  client.uploadSkin(effectiveDt, skinItems)
    .then(result => {
      console.log(result);
      const skinCreate = result.data.createSkin;

      const {skinItems} = skinCreate;
      const skinItemsFrmt = skinItems.map(function(si) {
        console.log(si)
        const userId = si.id
        return {
            part: si.part
          , pay: si.pay
          , email: si.email
          , callStart: si.callStartTs
          , callEnd: si.callEndTs
          , userId
          , firstName: si.firstName
          , lastName: si.lastName
        }
      })

      console.log(skinItemsFrmt)

      skinItemsFrmt.forEach(function(si) {
        client.createExtra(si.email, si.firstName, si.lastName, si.userId)
          .then(result => {
            console.log(result);
          })
          .catch(error => {
            console.error(error);
          })
      })

      const skin = {effectiveDt: skinCreate.effectiveDt, skinItems: skinItemsFrmt}
      app.ports.receiveDailySkin.send(skin);
    })
    .catch(error => {
      console.error(error);
    })
})

function uploadFile (event) {
  const file = event.target.files[0];
  const data = new window.FormData();
  data.append('data', file);

  const xhr = new window.XMLHttpRequest();
  xhr.open('POST', 'https://api.graph.cool/file/v1/ciykpioqm1wl00120k2e8s4la', true);
  xhr.onreadystatechange = () => {
    if (xhr.readyState === xhr.DONE) {
      const response = JSON.parse(xhr.responseText);
      client.updateWardrobeStatusFile(this.id, response.id)
        .then(result => app.ports.receiveWardrobeStatusUpdate.send(result))
        .then(window.alert('OK!'));
      event.target.removeEventListener('change', uploadFile);
      event.target.value = null;
    }
  };

  xhr.send(data);
}

app.ports.selectWardrobePhoto.subscribe((id) => {
  const node = document.getElementById(id);
  node.click();

  node.addEventListener('change', uploadFile);
});

app.ports.getAllWardrobeStatuses.subscribe(() => {
  client.getAllWardrobeStatuses()
    .then(result => app.ports.receiveWardrobeStatuses.send(result));
});

app.ports.checkInWardrobe.subscribe((id) => {
  client.checkInWardrobe(id)
    .then(result => app.ports.updateCheckStatus.send(result));
});

app.ports.checkOutWardrobe.subscribe((id) => {
  client.checkOutWardrobe(id)
    .then(result => app.ports.updateCheckStatus.send(result));
});
