/*
SAMPLE USAGE
------------
SELECT * FROM set_user_profile_photo('test@fake.com', 'http://samples3url...');

 */

CREATE OR REPLACE FUNCTION set_user_profile_photo (p_user_id text, p_s3_icon_url character varying) RETURNS character varying
	LANGUAGE sql
AS $$
UPDATE user_profile
SET s3_icon_avatar_url = p_s3_icon_url
RETURNING s3_icon_avatar_url

$$
