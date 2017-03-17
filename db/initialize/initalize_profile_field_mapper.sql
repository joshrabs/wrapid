CREATE FUNCTION initialize_profile_field_mapper () RETURNS TABLE(profile_field_id text, paper_form_template_field_id text)
	LANGUAGE sql
AS $$
    WITH map_insert AS
    (
      INSERT INTO profile_field_paper_template_form_field
        SELECT 'firstName' AS profile_field_id, 'firstName' AS paper_form_template_field_id
        UNION 
        SELECT 'lastName' AS profile_field_id, 'lastName' AS paper_form_template_field_id
      RETURNING profile_field_id, paper_form_template_field_id
    )
    SELECT
      profile_field_id,
      paper_form_template_field_id
    FROM map_insert
  
$$
