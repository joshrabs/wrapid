/*
SAMPLE USAGE:
-------------
SELECT
  user_id,
  clockin_ts,
  clockout_ts,
  wardrobe_checkin_ts,
  wardrobe_checkout_ts
FROM get_all_live_daily_extra_activity('2017-03-21', 'RunaBetterSet');

DROP FUNCTION get_all_live_daily_extra_activity(DATE, VARCHAR(100));
 */

CREATE OR REPLACE FUNCTION get_all_live_daily_extra_activity(p_effective_dt DATE, p_production_set_id VARCHAR(100))
  RETURNS
    TABLE(user_id VARCHAR(100)
    , clockin_ts TIMESTAMP
    , clockout_ts TIMESTAMP
    , wardrobe_checkin_ts TIMESTAMP
    , wardrobe_checkout_ts TIMESTAMP
    )
  AS
  $$
    SELECT
      eda.user_id,
      eda.clockin_ts::TIMESTAMP,
      eda.clockout_ts::TIMESTAMP,
      ewa.checkin_ts AS wardrobe_checkin_ts,
      ewa.checkout_ts AS wardrobe_checkout_ts
    FROM extra_daily_activity eda
    JOIN extra_wardrobe_status ewa
      ON ewa.effective_dt = eda.effective_dt
      AND ewa.production_set_id = eda.production_set_id
  $$
  LANGUAGE SQL
