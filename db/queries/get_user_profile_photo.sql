/*
SAMPLE USAGE:
--------------
SELECT * FROM get_user_profile_photo('test@fake.com');

 */

CREATE FUNCTION get_user_profile_photo (p_user_id text) RETURNS bytea
	LANGUAGE sql
AS $$
 SELECT
    up.profile_photo_b64 AS profile_photo_b64
  FROM "user"
  JOIN user_profile up
      ON up.user_id = "user".user_id
  WHERE up.user_id = p_user_id
$$
