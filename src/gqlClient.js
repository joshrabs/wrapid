import ApolloClient, { createNetworkInterface } from 'apollo-client';
import gql from 'graphql-tag';

const client = new ApolloClient({
  networkInterface: createNetworkInterface({
    uri: 'https://api.graph.cool/simple/v1/ciykpioqm1wl00120k2e8s4la'
  })
});

// Queries

const getAllProfiles = gql`query
  {
    allBaseProfiles {
      id
      firstName
      file {
        url
      }
    }
  }
`;

const getUserProfile = gql`query($userId: ID)
  {
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

const getUserSchedule = gql`query($userId: ID, $date: String)
  {
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

const createUser = gql`mutation($firstName: String!)
  {
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

const createSchedule = gql`mutation($date: String!, $title: String!, $startTm: String!)
  {
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

const clockinExtra = gql`mutation clockinExtra($id: ID!, $clockinTs: String)
  {
    updateTimecard(id: $id, clockinTs: $clockinTs)
    {
      effectiveDt
      clockinTs
      clockoutTs
    }
  }
`;

const clockoutExtra = gql`mutation clockoutExtra($id: ID!, $clockoutTs: String)
  {
    updateTimecard(id:$id, clockoutTs:$clockoutTs)
    {
      id
      effectiveDt
      clockinTs
      clockoutTs
    }
  }
`;

const getExtraInfo = gql`
  query getDaysExtraInfo($userId:ID, $date:String){
    User(id:$userId){
      baseprofile {
        firstName
        lastName
      },
      timecards(filter: {effectiveDt: $date}) {
        id,
        effectiveDt,
        clockinTs,
        clockoutTs
      }
    }
  }
`



const createTimecard = gql`mutation createTimecard($userId:ID!, $clockinTs:String, $clockoutTs:String, $effectiveDt:String!) {
  createTimecard(userId:$userId, clockinTs:$clockinTs, clockoutTs:$clockoutTs, effectiveDt:$effectiveDt){
    id,
    effectiveDt
  }
}`

export default {

  // Queries

  clockoutExtra: function(id, clockoutTs){
    const query = clockinExtra;
    const variables = { id, clockoutTs };
    console.log(query)
    console.log(variables)
    return client.mutate({ query, variables });
  },

  clockinExtra: function(id, clockinTs){
    const mutation = clockinExtra;
    const variables = { id, clockinTs};
    return client.mutate({ mutation, variables });
  },

  getExtraInfo: function(userId, date) {
    const query = getExtraInfo;
    console.log("QUERY!", query)
    const variables = { userId, date };
    return client.query({ query, variables });
  },

  createTimecard: function(userId, clockinTs, clockoutTs, effectiveDt) {
    const query = createTimecard;
    const variables = { userId, clockinTs, clockoutTs, effectiveDt };
    return client.query({ query, variables });
  },

  getAllProfiles: function () {
    const query = getAllProfiles;
    return client.query({ query });
  },

  getUserProfile: function (userId) {
    const query = getUserProfile;
    const variables = { userId };
    return client.query({ query, variables });
  },

  getUserSchedule: function (userId, date) {
    const query = getUserSchedule;
    const variables = { userId, date };
    return client.query({ query, variables });
  },

  // Mutations

  createUser: function (firstName) {
    const mutation = createUser;
    const variables = { firstName };
    return client.mutate({ mutation, variables });
  },

  createSchedule: function (date, title, startTm) {
    const mutation = createSchedule;
    const variables = { date, title, startTm };
    return client.mutate({ mutation, variables });
  }
};
