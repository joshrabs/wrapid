/*
SAMPLE USAGE:
--------------
SELECT * FROM upload_skin('2017-03-21', 'RunaBetterSet', 'j@fake.com,john,smith,08,30,cop,88/12;sa@fake.com,sally,miller,08,30,cop,88/12');

CHECK:
-------
SELECT * FROM get_daily_skin('2017-03-21');

MAINTENANCE:
-------------
DROP FUNCTION upload_skin(DATE, VARCHAR(100), TEXT );
TRUNCATE TABLE skin CASCADE;

 */

CREATE OR REPLACE FUNCTION upload_skin(p_effective_dt DATE, p_production_set_id VARCHAR(100), p_skin_items TEXT )
  RETURNS TABLE(unregistered_extras VARCHAR(100)) AS
  $$
    with skin_insert AS
      (
        INSERT INTO skin
          SELECT
            p_effective_dt AS effective_dt,
            p_production_set_id AS production_set_id
      ),
      itemRowStrings AS
      (
          SELECT unnest(string_to_array(p_skin_items, ';')) AS rs
      ),
      itemRowArrays AS
      (
          SELECT
              string_to_array(a.rs, ',') AS val
          FROM itemRowStrings AS a
      ),
      item_vals AS
        (
            SELECT
              ra.val[1] AS email,
              ra.val[2] AS first_name,
              ra.val[3] AS last_name,
              to_timestamp(to_char(p_effective_dt, 'YYYY-MM-DD') || ' ' ||ra.val[4] || ':' || ra.val[5], 'YYYY-MM-DD HH:MI') AS call_start_ts,
              ra.val[6] AS role,
              ra.val[7] AS pay
            FROM itemRowArrays AS ra
        ),
      skin_items_insert AS
        (
          INSERT INTO skin_item
            SELECT
              p_effective_dt AS effective_dt,
              p_production_set_id AS production_set_id,
              iv.email,
              iv.first_name,
              iv.last_name,
              iv.call_start_ts,
              iv.role,
              iv.pay
          FROM item_vals AS iv
        RETURNING email
        )
    SELECT
      sii.email AS unregistered_extras
    FROM skin_items_insert sii
    LEFT JOIN extra e ON e.user_id = sii.email
      AND e.user_id IS NULL
  $$
  LANGUAGE SQL
