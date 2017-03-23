/*
SAMPLE USAGE:
--------------
SELECT * FROM get_daily_skin('2017-03-21', 'RunaBetterSet');

MAINTENANCE:
------------
DROP FUNCTION get_daily_skin(DATE, VARCHAR);

 */

CREATE OR REPLACE FUNCTION get_daily_skin (p_effective_dt date, p_production_set_id character varying) RETURNS TABLE(effective_dt date, email character varying, full_name character varying, call_start_ts timestamp without time zone, role character varying, rate character varying)
	LANGUAGE sql
AS $$
    SELECT
      si.effective_dt,
      si.email,
      si.full_name,
      si.call_start_ts,
      si.role,
      si.rate
    FROM skin_item si
    WHERE si.effective_dt = p_effective_dt
      AND si.production_set_id = p_production_set_id

$$
