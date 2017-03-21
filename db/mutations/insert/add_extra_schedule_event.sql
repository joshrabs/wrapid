/*
SAMPLE USAGE:
---------------
SELECT * FROM
  add_extra_schedule_event
  ('2017-03-21'
  , 'RunaBetterSet'
  , 'test@fake.com,bobby@email.com'
  , 'Scene 1'
  , 'Act nicely'
  , to_timestamp('10:00', 'HH24:MI')
  , to_timestamp('13:00', 'HH24:MI')
  , 'INTERIOR'
  , 'DAY');


DROP FUNCTION add_extra_schedule_event(DATE, VARCHAR(50), TEXT, VARCHAR(100), VARCHAR(100), TIMESTAMP WITH TIME ZONE, TIMESTAMP WITH TIME ZONE, extra_schedule_event_scene_desc, extra_schedule_event_time_of_day);
 */

CREATE OR REPLACE FUNCTION
  add_extra_schedule_event(p_effective_dt DATE,
    p_production_set_id VARCHAR(50),
    p_extra_ids TEXT,
    p_title VARCHAR(100),
    p_description VARCHAR(100),
    p_start_ts TIMESTAMP WITH TIME ZONE,
    p_end_ts TIMESTAMP WITH TIME ZONE,
    p_scene_desc extra_schedule_event_scene_desc,
    p_time_of_day extra_schedule_event_time_of_day
  )
  RETURNS TEXT
  AS
  $$
  DECLARE
    p_schedule_return_insert ALIAS FOR $1;
  BEGIN
    --CHECK FIRST IF SCHEDULE EVENT EXISTS YET


   INSERT INTO extra_schedule
    SELECT
        p_effective_dt AS effective_dt
      , p_production_set_id AS production_set_id
      , extras.id AS user_id
      , now() AS created_ts
      , now() AS updated_ts
    FROM (
      SELECT unnest(string_to_array(p_extra_ids, ',')) AS id
      ) AS extras
    ON CONFLICT DO NOTHING;

    INSERT INTO extra_schedule_event
    SELECT
        p_effective_dt AS effective_dt
      , p_production_set_id AS production_set_id
      , p_title AS title
      , p_description AS description
      , p_start_ts AS start_ts
      , p_end_ts AS end_ts
      , p_scene_desc AS scene_desc
      , p_time_of_day AS time_of_day
      , now() AS created_ts
      , now() AS updated_ts
    ON CONFLICT DO NOTHING;

    INSERT INTO extra_schedule_event_item
    SELECT
        p_effective_dt AS effective_dt
      , p_production_set_id AS production_set_id
      , extras.id AS user_id
      , p_title AS title
      , p_start_ts AS start_ts
    FROM (
      SELECT unnest(string_to_array(p_extra_ids, ',')) AS id
      ) AS extras
    ON CONFLICT DO NOTHING;

    RETURN 'SUCESS'::TEXT;
  END;
  $$
  LANGUAGE PLPGSQL;
