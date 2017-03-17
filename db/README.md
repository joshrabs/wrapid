# wrapid-db

![alt text](./schematic/diagram.png "Logo Title Text 1")

## API

### Queries

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
set_user_profile_photo -> (p_user_id: TEXT, p_b64_image BYTEA) RETURNS BYTEA

####  Register extra
Adds extra as user and fills out some of profile fields (first name, last name)

register_extra: (user_id: TEXT, password_salt: TEXT, first_name: TEXT, last_name: TEXT) -> TABLE(user_id, profile_field_id, input)
```
SELECT user_id, profile_field_id, input) FROM register_extra('theraccoun@gmail.com', 'wrapid', 'Steven', 'MacCoun', 'J');
```
