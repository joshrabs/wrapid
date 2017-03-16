/*
SAMPLE USAGE:
  SELECT * FROM get_extra_profile('test@email.com')

-- DROP FUNCTION get_extra_profile(text);
 */

CREATE OR REPLACE FUNCTION get_extra_profile (p_user_id text)
  RETURNS TABLE(user_id TEXT, profile_field_id TEXT, label TEXT, value TEXT)
AS $$
   SELECT
    extra.user_id,
    pf.profile_field_id,
    pf.label,
    pfi.input
  FROM "user"
  JOIN extra ON extra.user_id = "user".user_id
    AND extra.user_id = p_user_id
  JOIN user_profile up ON up.user_id = extra.user_id
  JOIN profile_field_input pfi ON pfi.user_id = up.user_id
  JOIN profile_field pf ON pf.profile_field_id = pfi.profile_field_id;
$$
  LANGUAGE sql;





