CREATE FUNCTION initialize_db () RETURNS integer
	LANGUAGE plpgsql
AS $$
  BEGIN
    SELECT initialize_paper_form_templates();
    SELECT initialize_profile_fields();
    SELECT initialize_profile_field_mapper();
    RETURN 1;
  END;
  
$$
