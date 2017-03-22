/*
SAMPLE USAGE:
--------------
SELECT * FROM get_user_profile_photo('test@fake.com');

DROP FUNCTION get_user_profile_photo(text);
 */

CREATE OR REPLACE FUNCTION get_user_profile_photo (p_user_id text) RETURNS VARCHAR(500)
	LANGUAGE sql
AS $$
 SELECT
    up.s3_icon_avatar_url AS profile_photo_b64
  FROM "user"
  JOIN user_profile up
      ON up.user_id = "user".user_id
  WHERE up.user_id = p_user_id
$$
