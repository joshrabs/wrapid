/*
SELECT * FROM upload_skin('2017-03-23', 'RunaBetterSet'
, 'j@fake.com,john smith,08,30,cop,88/12,BG,Make 2012;sa@fake.com,sally miller,08,30,cop,88/12,SA,Make 2010');

DROP FUNCTION upload_skin(DATE, VARCHAR, TEXT);
*/

CREATE OR REPLACE  FUNCTION upload_skin (p_effective_dt date, p_production_set_id character varying, p_skin_items text)
  RETURNS TABLE(unregistered_extras character varying)
AS $$
  BEGIN
      INSERT INTO skin
        SELECT
          p_effective_dt AS effective_dt,
          p_production_set_id AS production_set_id;


      DROP TABLE IF EXISTS item_row_arrays;
      CREATE TEMPORARY TABLE item_row_arrays AS
        WITH itemRowStrings AS
        (
          SELECT unnest(string_to_array(p_skin_items, ';')) AS rs
        )
        SELECT
            string_to_array(a.rs, ',') AS val
        FROM itemRowStrings AS a;

     --Extra's are always inserted, even if not registered--
      INSERT INTO "user"
      SELECT
        ra.val[1] AS user_id
      FROM item_row_arrays AS ra
      ON CONFLICT DO NOTHING;

      INSERT INTO extra
      SELECT
        ra.val[1] AS user_id
      FROM item_row_arrays AS ra
      ON CONFLICT DO NOTHING;


        INSERT INTO skin_item
          SELECT
            p_effective_dt AS effective_dt,
            p_production_set_id AS production_set_id,
            iv.email,
            iv.call_start_ts,
            iv.role,
            iv.full_name,
            iv.extra_talent_type,
            iv.notes,
            iv.rate
        FROM (
          SELECT
              ra.val[1] AS email,
              ra.val[2] AS full_name,
              to_timestamp(to_char(p_effective_dt, 'YYYY-MM-DD') || ' ' ||ra.val[3] || ':' || ra.val[4], 'YYYY-MM-DD HH:MI') AS call_start_ts,
              ra.val[5] AS role,
              ra.val[6] AS rate,
              ra.val[7] AS extra_talent_type,
              ra.val[8] AS notes
            FROM item_row_arrays AS ra
        ) AS iv;

    RETURN QUERY
    SELECT
      sii.email::VARCHAR AS unregistered_extras
    FROM skin_item sii
    LEFT JOIN login e ON e.user_id = sii.email
      AND e.user_id IS NULL
    WHERE p_effective_dt = sii.effective_dt;
  END;
$$
LANGUAGE PLPGSQL
