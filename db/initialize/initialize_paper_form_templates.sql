CREATE FUNCTION initialize_paper_form_templates () RETURNS TABLE(paper_form_template_id text, label text, owner text, coord_x integer, coord_y integer)
  LANGUAGE sql
AS $$
    WITH efs_base AS
    (
      INSERT INTO paper_form_template VALUES ('EFS', NULL)
      RETURNING paper_form_template_id
    ),
    efs_field_map AS (
        SELECT 'firstName' AS paper_field_template_id, 'First Name' AS label, 'OWNER' AS owner, 3 AS coord_x, 5 AS coord_y
        UNION
        SELECT 'lastName' AS paper_field_template_id, 'Last Name' AS label, 'OWNER' AS owner, 3 AS coord_x, 15 AS coord_y
    ),
    efs_fields AS (
      INSERT INTO paper_form_template_field
      SELECT
        m.paper_field_template_id,
        m.label,
        m.owner,
        m.coord_x,
        m.coord_y,
        efs_base.paper_form_template_id
      FROM efs_field_map m
      CROSS JOIN efs_base
      RETURNING *
    )
    SELECT
      paper_form_template_id,
      label,
      owner,
      coord_x,
      coord_y
    FROM efs_fields
  
$$
