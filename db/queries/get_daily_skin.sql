/*
SAMPLE USAGE:
--------------
SELECT * FROM get_daily_skin('2017-03-20');


DROP FUNCTION get_daily_skin(DATE);
 */

CREATE OR REPLACE FUNCTION get_daily_skin(p_effective_dt DATE)
  RETURNS TABLE (effective_dt DATE, email VARCHAR(100), first_name VARCHAR(100), last_name VARCHAR(100), call_start_ts TIMESTAMP, role VARCHAR(50), pay VARCHAR(50))
  AS
  $$
    SELECT
      si.effective_dt,
      si.email,
      si.first_name,
      si.last_name,
      si.call_start_ts,
      si.role,
      si.pay
    FROM skin_item si
    WHERE si.effective_dt = p_effective_dt
  $$
  LANGUAGE SQL
