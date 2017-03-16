/*
SAMPLE USAGE:
  SELECT user_id, profile_field_id, input FROM register_extra('test@fake.com', 'password', 'John', 'Smith', 'J');


VERIFY:

  SELECT * FROM get_extra_profile('test@fake.com');

CLEANUP:
--DROP FUNCTION register_extra(text, text, text, text, text);
 */

CREATE OR REPLACE FUNCTION register_extra (p_user_id text, p_password_salt text, p_first_name text, p_last_name text, p_middle_initial text)
  RETURNS TABLE(user_id TEXT, profile_field_id TEXT, input TEXT)
AS $$

  WITH user_insert AS (
    INSERT INTO public."user" VALUES (p_user_id, p_password_salt)
    RETURNING user_id
  ),
  extra_insert AS (
    INSERT INTO public.extra
      SELECT user_insert.user_id AS user_id
      FROM user_insert
    RETURNING user_id
  ),
  user_profile_insert AS (
    INSERT INTO user_profile
      SELECT
        extra_insert.user_id AS user_id,
        NULL AS last_submitted_ts
      FROM extra_insert
    RETURNING user_id, last_submitted_ts
  ),
  profile_field_input_insert AS (
    INSERT INTO profile_field_input
      SELECT
        pf.profile_field_id,
        map.input AS input,
        now() AS updated_ts,
        nextval('profile_field_input_profile_field_input_id_seq') AS profile_field_input_id,
        p_user_id AS user_id
      FROM profile_field pf
      JOIN
        (SELECT 'firstName' AS profile_field_id, p_first_name AS input
          UNION
          SELECT 'lastName' AS profile_field_id, p_last_name AS input
        ) map
        ON map.profile_field_id = pf.profile_field_id
    RETURNING profile_field_input_id, profile_field_id, input, updated_ts, user_id
  )
  SELECT upfi.user_id, pf.label, upfi.input
  FROM profile_field_input_insert upfi
  JOIN profile_field pf ON pf.profile_field_id = upfi.profile_field_id
  JOIN extra ON extra.user_id = upfi.user_id
$$
LANGUAGE sql;


