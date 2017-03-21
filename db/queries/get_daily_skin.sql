/*
SAMPLE USAGE:
--------------
SELECT
  effective_dt,
  email,
  full_name
  call_start_ts,
  role,
  rate,
  extra_talent_type,
  notes
FROM get_daily_skin('2017-03-20');


DROP FUNCTION get_daily_skin(DATE);
 */

CREATE OR REPLACE FUNCTION get_daily_skin(p_effective_dt DATE)
  RETURNS
    TABLE (
      effective_dt DATE
    , email VARCHAR(100)
    , full_name VARCHAR(100)
    , call_start_ts TIMESTAMP WITHOUT TIME ZONE
    , role VARCHAR(50)
    , rate VARCHAR(50)
    , extra_talent_type VARCHAR(50)
    , notes TEXT
    )
  AS
  $$
    SELECT
      si.effective_dt,
      si.email,
      si.full_name,
      si.call_start_ts,
      si.role,
      si.rate,
      si.extra_talent_type,
      si.notes
    FROM skin_item si
    WHERE si.effective_dt = p_effective_dt
  $$
  LANGUAGE SQL
