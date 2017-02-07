import { Lokka } from 'lokka';
import { Transport } from 'lokka-transport-http';

const client = new Lokka({
  transport: new Transport('https://api.graph.cool/simple/v1/ciykpioqm1wl00120k2e8s4la')
});

// Queries

const getAllProfiles = `
  query {
    allBaseProfiles {
      id
      firstName
      file {
        url
      }
    }
  }
`;

const getUserProfile = `
  query ($userId: ID) {
    User(id: $userId) {
      baseprofile {
        id
        firstName
        file {
          url
        }
      }
    }
  }
`;

const getUserSchedule = `
  query ($userId: ID, $date: String) {
    allExtraScheduleItemses(
      filter: {
        extraschedule: {
          date: $date
          user: { id: $userId }
        }
      }
    ) {
      title
      startTm
    }
  }
`;

// Mutations

const createUser = `
  ($firstName: String!) {
    createUser(
      baseprofile: {
        firstName: $firstName
      }
    ) {
      id
      baseprofile {
        firstName
      }
    }
  }
`;

const createSchedule = `
  ($date: String!, $title: String!, $startTm: String!) {
    createExtraSchedule(
      date: $date
      extrascheduleitemses: {
        title: $title
        startTm: $startTm
      }
    ) {
      id
    }
  }
`;

export default {

  // Queries

  getAllProfiles: function () {
    return client.query(getAllProfiles);
  },

  getUserProfile: function (userId) {
    return client.query(getUserProfile, { userId: userId });
  },

  getUserSchedule: function (userId, date) {
    return client.query(getUserSchedule, { userId: userId, date: date });
  },

  // Mutations

  createUser: function (firstName) {
    return client.mutate(createUser, { firstName: firstName });
  },

  createSchedule: function (date, title, startTm) {
    return client.mutate(
      createSchedule,
      {
        date: date,
        title: title,
        startTm: startTm
      }
    );
  }
};
