/*
SAMPLE USAGE:
--------------
SELECT
  unregistered_extras
FROM create_skin('2017-03-21', 'RunaBetterSet'
, 'j@fake.com,john smith,08,30,cop,88/12,BG,Make 2012;sa@fake.com,sally miller,08,30,cop,88/12,SA,Make 2010');

CHECK
------
SELECT * FROM get_daily_skin('2017-03-21', 'RunaBetterSet');
SELECT * FROM get_daily_extra_activity('2017-03-21', 'RunaBetterSet');

MAINTENANCE:
-------------
DROP FUNCTION create_skin(DATE, VARCHAR, TEXT);
TRUNCATE TABLE skin CASCADE;
TRUNCATE TABLE extra_daily_activity CASCADE;

*/

CREATE OR REPLACE FUNCTION create_skin(p_effective_dt date, p_production_set_id character varying, p_skin_items text) RETURNS TABLE(unregistered_extras character varying)
	LANGUAGE plpgsql
AS $$
  BEGIN
      INSERT INTO skin
        SELECT
          p_effective_dt AS effective_dt,
          p_production_set_id AS production_set_id;


      DROP TABLE IF EXISTS temp_skin_items;
      CREATE TEMPORARY TABLE temp_skin_items AS
        WITH itemRowStrings AS
        (
          SELECT unnest(string_to_array(p_skin_items, ';')) AS rs
        ),
        ra AS (
          SELECT
              string_to_array(a.rs, ',') AS val
          FROM itemRowStrings AS a
        )
        SELECT
              ra.val[1] AS email,
              ra.val[2] AS full_name,
              to_timestamp(to_char(p_effective_dt, 'YYYY-MM-DD') || ' ' ||ra.val[3] || ':' || ra.val[4], 'YYYY-MM-DD HH:MI') AS call_start_ts,
              ra.val[5] AS role,
              ra.val[6] AS rate,
              ra.val[7] AS extra_talent_type,
              ra.val[8] AS notes
            FROM ra;

     --Extra's are always inserted, even if not registered--
      INSERT INTO "user"
      SELECT
        ra.email AS user_id
      FROM temp_skin_items AS ra
      ON CONFLICT DO NOTHING;

      INSERT INTO extra
      SELECT
        ra.email AS user_id
      FROM temp_skin_items AS ra
      ON CONFLICT DO NOTHING;

      --Extra Daily Activities are Prepped
      INSERT INTO extra_daily_activity
        SELECT
          ra.email AS user_id,
          p_effective_dt AS effective_dt,
          p_production_set_id AS production_set_id,
          null AS clockin_ts,
          null AS clockout_ts,
          null AS meal_start_ts
        FROM temp_skin_items AS ra;

      --Add default rows for extra wardrobe status
      INSERT INTO extra_wardrobe_status
        SELECT
          ra.email AS user_id,
          p_effective_dt AS effective_dt,
          p_production_set_id AS production_set_id,
          null::TIMESTAMP WITH TIME ZONE AS checkin_ts,
          null::TIMESTAMP WITH TIME ZONE AS checkout_ts
        FROM temp_skin_items AS ra;


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
        FROM temp_skin_items AS iv;

    RETURN QUERY
    SELECT
      sii.email::VARCHAR AS unregistered_extras
    FROM skin_item sii
    LEFT JOIN login e ON e.user_id = sii.email
      AND e.user_id IS NULL
    WHERE p_effective_dt = sii.effective_dt;
  END;
$$
