/*
SAMPLE USAGE:
-------------
SELECT * FROM create_meal('2017-03-21', 'RunaBetterSet', '14:30', 30, 'j@fake.com,sa@fake.com');


VALIDATE:
---------
SELECT user_id, meal_start_ts
FROM extra_daily_activity eda
WHERE user_id=ANY(string_to_array('j@fake.com,sa@fake.com', ','));

MAINTENANCE:
-----------
DROP FUNCTION create_meal(DATE, VARCHAR, TEXT, INT, TEXT);
 */

CREATE OR REPLACE FUNCTION create_meal(p_effective_dt DATE, p_production_set_id VARCHAR(100), p_start_ts TEXT, p_duration INT, p_user_id_arr TEXT)
  RETURNS BIGINT AS
  $$
  DECLARE
    v_affected_rows BIGINT;
    v_meal_start_ts TIMESTAMP;
  BEGIN

    v_meal_start_ts = to_timestamp(p_start_ts, 'HH24:MI');

      INSERT INTO meal
          SELECT
            p_effective_dt AS effective_dt,
            p_production_set_id AS production_set_id,
            v_meal_start_ts AS start_ts,
            p_duration AS duration
      ON CONFLICT DO NOTHING;

      UPDATE extra_daily_activity
          SET meal_start_ts = v_meal_start_ts
          WHERE user_id=ANY(string_to_array(p_user_id_arr, ','));

      GET DIAGNOSTICS v_affected_rows = ROW_COUNT;

      RETURN v_affected_rows;
  END;
  $$
  LANGUAGE PLPGSQL;
