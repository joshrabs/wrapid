import ApolloClient, { createNetworkInterface } from 'apollo-client';
import {SubscriptionClient, addGraphQLSubscriptions} from 'subscriptions-transport-ws'
import gql from 'graphql-tag';

const wsClient = new SubscriptionClient(`wss://subscriptions.graph.cool/v1/ciykpioqm1wl00120k2e8s4la`, {
  reconnect: true,
  connectionParams: {
    // Pass any arguments you want for initialization
  }
})

const networkInterface = createNetworkInterface({ uri: 'https://api.graph.cool/simple/v1/ciykpioqm1wl00120k2e8s4la' })

// Extend the network interface with the WebSocket
const networkInterfaceWithSubscriptions = addGraphQLSubscriptions(
  networkInterface,
  wsClient
)

const client = new ApolloClient({
  networkInterface: networkInterfaceWithSubscriptions
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

const addScheduleItemGQL = gql`mutation

addScheduleByRole(
  	$scheduleId:ID
  , $name:String!
  , $category:EXTRA_SCHEDULE_ITEMS_CATEGORY
  , $startTm:String!
  , $endTm:String
){
  createExtraScheduleItems(
    extrascheduleId:$scheduleId
    , name:$name
    , category:$category
    , startTm:$startTm
    , endTm:$endTm
  ){
    id
    name
  }
}`

const subscribeSchedule = gql`subscription {
  ExtraScheduleItems{
    mutation
    node {
      name
      category
      startTm
      endTm
    }
  }
}`

const clockinExtra = gql`mutation clockinExtra($id: ID!, $clockinTs: String)
  {
    updateTimecard(id: $id, clockinTs: $clockinTs)
    {
      id,
      effectiveDt,
      clockinTs,
      clockoutTs
    }
  }
`;

const clockoutExtra = gql`mutation clockoutExtra($id: ID!, $clockoutTs: String)
  {
    updateTimecard(id:$id, clockoutTs:$clockoutTs)
    {
      id,
      effectiveDt,
      clockinTs,
      clockoutTs
    }
  }
`;

const getExtraInfo = gql`
  query getDaysExtraInfo($userId:ID, $date:String){
    User(id:$userId){
      baseprofile {
        avatar { url }
        firstName
        lastName
      },
      extraschedule (filter: {date:$date}){
        id
        extrascheduleitemses(orderBy:startTm_ASC){
          name
          category
          startTm
          endTm
        }
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



const paLiveMonitorAllExtraInfo = gql`query getAllExtraInfo {
  allUsers(filter:{employeeType:Extra}){
    id
    baseprofile{
      firstName
      lastName
      avatar {
        url
      }
    }
    timecards(filter:{effectiveDt:"2014-07-18"}){
      id
      effectiveDt
      clockinTs
      clockoutTs
    }
    extraschedule{
      id
      extrascheduleitemses{
        name
        category
        startTm
        endTm
      }
    }
  }
}`

const fetchDailySkinGQL = gql`query getDailySkin($date:String){
  allSkins(filter: {effectiveDt:$date}){
    skinItems{
      user{
        id
        baseprofile{
          firstName
          lastName
        }
      }
      role
      pay
    }
  }
}`

export default {

  //subscriptions
  subExtraSchedule: function(){
    const query = subscribeSchedule;
    const variables = {};
    console.log(client)
    console.log(query)
    return client.query({ query, variables });
  },

  // Queries
  getAllExtraInfo: function(date) {
    const query = paLiveMonitorAllExtraInfo;
    console.log("QUERY!", query)
    const variables = {};
    return client.query({ query, variables });
  },

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

  fetchDailySkin: function(date) {
    const query = fetchDailySkinGQL;
    const variables = { date };
    return client.query({ query, variables });
  },

  getExtraInfo: function(userId, date) {
    const query = getExtraInfo;
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
  },

  addScheduleItem: function(scheduleItem) {
    const mutation = addScheduleItemGQL
    const {startTm, endTm, category, name} = scheduleItem
    const variables = {scheduleId: "cizbzpnmaw6m80148bpys69a6", startTm, endTm, category, name}
    return client.mutate({mutation, variables})
  }
};
