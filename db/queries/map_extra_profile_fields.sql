/*
SAMPLE USAGE:
------------

SELECT * FROM map_extra_profile_fields('test@fake.com');
 */

CREATE FUNCTION map_extra_profile_fields (p_user_id text) RETURNS TABLE(user_id text, paper_form_template_id text, value text, label text, coord_x integer, coord_y integer)
  LANGUAGE sql
AS $$
    SELECT
      ep.user_id,
      pft.paper_form_template_id,
      ep.value,
      pftf.label,
      pftf.coord_x,
      pftf.coord_y
    FROM get_extra_profile(p_user_id) ep
    JOIN profile_field_paper_template_form_field map ON map.profile_field_id = ep.profile_field_id
    JOIN paper_form_template_field pftf ON pftf.paper_form_template_field_id = map.paper_form_template_field_id
    JOIN paper_form_template pft ON pft.paper_form_template_id = pftf.paper_form_template_id
  
$$