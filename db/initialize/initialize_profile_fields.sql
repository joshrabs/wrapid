/*
SAMPLE USAGE:
-------------
SELECT * FROM initialize_profile_fields();

--DROP FUNCTION initialize_profile_fields();
 */

CREATE OR REPLACE FUNCTION initialize_profile_fields() RETURNS TABLE(profile_field_id text, label text)
	LANGUAGE sql
AS $$
    WITH map_insert AS
    (
      INSERT INTO profile_field
        SELECT 'firstName' AS profile_field_id, 'First Name' AS label
        UNION
        SELECT 'lastName' AS profile_field_id, 'Last Name' AS label
      RETURNING profile_field_id, label
    )
    SELECT
      profile_field_id,
      label
    FROM map_insert

$$
