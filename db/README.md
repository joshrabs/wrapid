# wrapid-db

![alt text](./schematic/diagram.png "Logo Title Text 1")

## API

### Queries

#### Get daily skin
*name: * get_daily_skin(p_effective_dt DATE)

*RETURNS TABLE*
(effective_dt DATE,
  email VARCHAR(100)
  , first_name VARCHAR(100)
  , last_name VARCHAR(100)
  , call_start_ts TIMESTAMP
  , role VARCHAR(50)
  , pay VARCHAR(50
))

```
SELECT
  effective_dt,
  email,
  first_name,
  last_name,
  call_start_ts,
  role,
  pay
FROM get_daily_skin('2017-03-20');
```

#### Get User Profile Picture
get_user_profile_photo -> (user_id TEXT) -> BYTEA
```
SELECT * FROM get_user_profile_photo('test@fake.com');
```

#### Get Extra profile

get_extra_profile -> (user_id: TEXT) -> TABLE()
```
SELECT * FROM get_extra_profile('test@email.com');
```

#### Get Extra Form from Mapped Profile

map_extra_profile_fields -> (user_id: TEXT) -> TABLE()
```
SELECT * FROM map_extra_profile_fields('test@email.com');
```


### Mutations

#### Set User Profile Photo
set_user_profile_photo -> (p_user_id: TEXT, p_b64_image BYTEA) -> VARCHAR(500)

```
SELECT * FROM set_user_profile_photo('test@fake.com', 'http://samples3url...');
```

#### Upload Skin
upload_skin(p_effective_dt DATE, p_production_set_id VARCHAR(100), p_skin_items TEXT )

*PARAMETERS*
p_skin_items is a comma separated row of each skin item, where rows are separated by semicolons
p_skin_items = '<email>,<first_name>,<last_name>,<callStartHH>,<callEndHH>,<role>,<pay>;......'

RETURNS TABLE(unregistered_extras VARCHAR(100)

```
SELECT * FROM upload_skin('2017-03-21', 'RunaBetterSet', 'j@fake.com,john,smith,08,30,cop,88/12;sa@fake.com,sally,miller,08,30,cop,88/12');
```

####  Register extra
Adds extra as user and fills out some of profile fields (first name, last name)

register_extra: (user_id: TEXT, password_salt: TEXT, first_name: TEXT, last_name: TEXT) -> TABLE(user_id, profile_field_id, input)
```
SELECT user_id, profile_field_id, input) FROM register_extra('theraccoun@gmail.com', 'wrapid', 'Steven', 'MacCoun', 'J');
```
