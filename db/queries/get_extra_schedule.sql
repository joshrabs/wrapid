/*
SAMPLE USAGE:
-------------
SELECT * FROM get_extra_schedule('2017-03-21', 'test@fake.com');

 */
CREATE OR REPLACE FUNCTION get_extra_schedule(p_effective_dt DATE, p_user_id VARCHAR(100))
  RETURNS TABLE(
    effective_dt DATE,
    production_set_id VARCHAR(50),
    user_id TEXT,
    title VARCHAR(100),
    description VARCHAR(100),
    start_ts TIMESTAMP WITHOUT TIME ZONE,
    end_ts TIMESTAMP WITHOUT TIME ZONE,
    scene_desc extra_schedule_event_scene_desc,
    time_of_day extra_schedule_event_time_of_day
  )
  AS
  $$
    SELECT
       ese.effective_dt AS effective_dt
      , ese.production_set_id AS production_set_id
      , ese.user_id AS user_id
      , ese.title AS title
      , ese.description AS description
      , ese.start_ts AS start_ts
      , ese.end_ts AS end_ts
      , ese.scene_desc AS scene_desc
      , ese.time_of_day AS time_of_day
    FROM extra_schedule es
    JOIN extra_schedule_event ese
        ON es.effective_dt = ese.effective_dt
        AND es.production_set_id = ese.production_set_id
        AND es.user_id = ese.user_id
    WHERE es.effective_dt = p_effective_dt
        AND es.user_id = p_user_id
  $$
  LANGUAGE SQL
