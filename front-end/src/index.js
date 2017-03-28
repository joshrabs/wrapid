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
  getAndSendExtraInfo(userId, day);
});

function getAndSendExtraInfo (userId, day) {
  console.log(userId);
  console.log(day);
  client.getExtraInfo(userId, day)
    .then(result => {
      console.log(result);
      const skin = result.data.allSkins[0];
      if (skin.length != 1) {
        // error!
      }
      console.log(skin);
      const skinItem = skin.skinItems[0];
      console.log(skinItem);
      let {baseprofile, timecards, extraschedule} = skin.skinItems[0].user;
      console.log(baseprofile);
      console.log('timecards!', timecards);
      const timecard = timecards.length > 0 ? timecards[0] : null;
      console.log(timecard);
      const scheduleItems = extraschedule.extrascheduleitemses;
      const schedule = {id: extraschedule.id, items: scheduleItems};
      let {firstName, lastName, avatar} = baseprofile;

      if (!avatar) {
        avatar = {url: null};
      }
      const profile = {firstName, lastName, avatar};
      const extraInfo = {extraId: userId, timecard, profile, schedule};
      app.ports.receiveExtraInfo.send(extraInfo);
    })
    .catch(error => {
      console.log(error);
    });
}

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
  const name = scheduleParams[1];
  const startTm = scheduleParams[2];
  client.createSchedule(date, name, startTm)
    .then(result => {
      console.log('result!', result);
      const timecard = result.data;
      // app.ports.receiveExtraInfo.send(timecard);
    })
    .catch(error => {
      console.log(error);
    });
});

app.ports.getAllExtraInfo.subscribe(function (d) {
  console.log(d);
  client.getAllExtraInfo(d)
    .then(result => {
      console.log(result);
      let allUsers = result.data.allSkins[0].skinItems;
      console.log(allUsers);
      let mapInfo = function (si) {
        const extra = si.user;
        console.log(extra);
        const {part, pay} = si;
        let {id, baseprofile, extraschedule, timecards} = extra;
        let {firstName, lastName, avatar, email} = baseprofile;
        let avatarSrc = avatar ? avatar.url : null;
        let profile = {firstName, lastName, avatar: {url: avatarSrc}, extraId: email, role: part, pay};
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

        let frmt = {
          extraId: id
          , firstName: profile.firstName
          , lastName: profile.lastName
          , avatar: profile.avatar
          , role: profile.role
          , pay: profile.pay
          ,  schedule
          , timecard: defTimecard
        };
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
      if (!skinResult) {
        app.ports.receiveDailySkin.send(null);
        return;
      }
      const {skinItems} = skinResult;
      const frmtSkinItems = skinItems.map(function (item) {
        console.log(item);
        const {user, part, pay, callStartTs, email} = item;
        const {firstName, lastName, avatar} = user.baseprofile;
        let sAvatar = avatar;
        if (!avatar) {
          sAvatar = {url: null};
        }
        console.log(sAvatar);
        return {email, firstName, lastName, part, pay, avatar: sAvatar, callStart: callStartTs};
      });
      console.log(frmtSkinItems);
      const skin = {effectiveDt: date, skinItems: frmtSkinItems};
      console.log(skin);
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

app.ports.uploadSkin.subscribe(function (params) {
  console.log(params);
  let {effectiveDt, skinItems} = params;
  console.log(effectiveDt);
  console.log(skinItems);
  skinItems = skinItems.map(function (si) {
    return {
      'callStartTs': si.callStart,
      'callEndTs': '',
      pay: si.pay,
      part: si.part,
      email: si.email,
      firstName: si.firstName,
      lastName: si.lastName
    };
  });
  console.log(skinItems);
  client.uploadSkin(effectiveDt, skinItems)
    .then(result => {
      console.log(result);
      const skinCreate = result.data.createSkin;

      const {skinItems} = skinCreate;
      const skinItemsFrmt = skinItems.map(function (si) {
        console.log(si);
        const userId = si.id;
        return {
          part: si.part,
          pay: si.pay,
          email: si.email,
          callStart: si.callStartTs,
          callEnd: si.callEndTs,
          userId,
          firstName: si.firstName,
          lastName: si.lastName
        };
      });

      console.log(skinItemsFrmt);

      skinItemsFrmt.forEach(function (si) {
        client.createExtra(si.email, si.firstName, si.lastName, si.userId)
          .then(result => {
            console.log(result);
            const userId = result.data.createUser.id;
            console.log(si);
            client.createSchedule(userId, effectiveDt, 'Arrive on Set', si.callStart)
              .then(result => {
                console.log('result!', result);
                const schedule = result.data;
                // app.ports.receiveExtraInfo.send(timecard);
              })
              .catch(error => {
                console.log(error);
              });

            client.createTimecard(userId, null, null, effectiveDt)
              .then(result => {
                console.log('timecard created!', result);
              })
              .catch(error => {
                console.error(error);
              });

            client.createExtraWardrobeCard(userId, effectiveDt)
              .then(result => {
                console.log('Created wardrobe card!', result);
              })
              .catch(error => console.error(error));
          })
          .catch(error => {
            console.error(error);
          });
      });

      const skin = {effectiveDt: skinCreate.effectiveDt, skinItems: skinItemsFrmt};
      app.ports.receiveDailySkin.send(skin);
    })
    .catch(error => {
      console.error(error);
    });
});

function uploadFileClick (id, onUploadFuncName) {
  console.log(id);
  const node = document.getElementById(id);
  node.addEventListener('change', (e) => uploadFileListen(e, id, onUploadFuncName));
  node.click();
}

function uploadFileListen (event, id, serverCall) {
  console.log(event);
  console.log(id);

  const file = event.target.files[0];
  const data = new window.FormData();
  data.append('data', file);

  console.log(data)
  serverCall(id, file, data)
}

const graphCoolCall = (id, file, data, onUploadFuncName) => {
  const xhr = new window.XMLHttpRequest();
  xhr.open('POST', 'https://api.graph.cool/file/v1/ciykpioqm1wl00120k2e8s4la', true);
  xhr.onreadystatechange = () => {
    if (xhr.readyState === xhr.DONE) {
      const response = JSON.parse(xhr.responseText);
      console.log(response);
      uploadPortMap[onUploadFuncName](id, response);
    }
  };

  xhr.send(data);
}

const updateWardrobeStatusFile = (id, response) => {
  console.log('Status ID: ', id);
  console.log('Response ID: ', response.id);
  client.updateWardrobeStatusFile(id, response.id)
    .then(result => app.ports.receiveWardrobeStatusUpdate.send(result));
  event.target.removeEventListener('change', (e) => uploadFileListen(e, id, onUploadFuncName));
  event.target.value = null;
};

const onAvatarUpload = (id, response) => {
  client.updateUserAvatar('cj0bdii6lzzjc0112yc07ez5h', id)
    .then(result => {
      console.log(result);
      // getAndSendExtraInfo('Bob@fakeguy.com', '2017-03-15')
    });

  event.target.removeEventListener('change', (e) => uploadFileListen(e, id, onUploadFuncName));
  event.target.value = null;
};

const sendSkinCSV = (id, file, data, date) => {
  console.log(id)
  console.log(file)
  console.log(data)
  console.log(date)

  const xhr = new window.XMLHttpRequest();
  const baseUrl = 'https://api.runabetterset.com/1/upload/set/runabetterset/skin/'
  xhr.open('POST', baseUrl + date , true);
  xhr.setRequestHeader("Content-type", "application/x-www-form-urlencoded");
  xhr.onreadystatechange = () => {
    if (xhr.readyState === xhr.DONE) {
      const response = JSON.parse(xhr.responseText);
      console.log(response);
      onSkinCSVUpload(file)
    }
  };

  xhr.send(file);
}

const onSkinCSVUpload = (file) => {

  client.uploadSkinCSV(file);

  event.target.removeEventListener('change', (e) => uploadFileListen(e, id, onUploadFuncName));
  event.target.value = null;
};

const uploadPortMap =
  {updateWardrobeStatusFile: (id, response) => updateWardrobeStatusFile(id, response),
    onAvatarUpload: (id, response) => onAvatarUpload(id, response)
  };

app.ports.uploadAvatar.subscribe((id) => {
  console.log(id);
  uploadFileClick(id, (id, data) => graphCoolCall(id, data, 'onAvatarUpload'));
});

app.ports.selectWardrobePhoto.subscribe(id => {
  uploadFileClick(id, (id, data) => graphCoolCall(id, data, 'updateWardrobeStatusFile'));
});

app.ports.uploadSkinFile.subscribe((param) => {
  const id = param[0]
  const date = param[1]
  uploadFileClick(id, (id, file, data) => sendSkinCSV(id, file, data, date));
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
