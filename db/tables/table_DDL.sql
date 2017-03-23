CREATE TYPE extra_schedule_event_scene_desc AS ENUM ('INTERIOR', 'EXTERIOR');
CREATE TYPE extra_schedule_event_time_of_day AS ENUM ('DAY', 'NIGHT');

CREATE TYPE extra_talent_type AS ENUM ('BG', 'SA', 'SI', 'PD');

CREATE SEQUENCE profile_field_input_profile_field_input_id_seq STARTS AT 1;

CREATE TABLE extra
(
    user_id VARCHAR(100) PRIMARY KEY NOT NULL,
    created_ts TIMESTAMP WITH TIME ZONE DEFAULT now() NOT NULL,
    updated_ts TIMESTAMP WITH TIME ZONE DEFAULT now() NOT NULL
);
CREATE TABLE extra_daily_activity
(
    user_id VARCHAR(100) NOT NULL,
    effective_dt DATE NOT NULL,
    production_set_id VARCHAR(50) NOT NULL,
    clockin_ts TIMESTAMP WITH TIME ZONE,
    clockout_ts TIMESTAMP,
    meal_start_ts TIMESTAMP,
    CONSTRAINT extra_daily_activity_user_id_production_set_id_effective_dt_pk PRIMARY KEY (user_id, production_set_id, effective_dt)
);
CREATE TABLE extra_schedule
(
    effective_dt DATE NOT NULL,
    production_set_id VARCHAR(50) NOT NULL,
    user_id VARCHAR(100) NOT NULL,
    created_ts TIMESTAMP WITH TIME ZONE DEFAULT now() NOT NULL,
    updated_ts TIMESTAMP WITH TIME ZONE DEFAULT now() NOT NULL,
    CONSTRAINT extra_schedule_effective_dt_production_shoot_lat_production_sho PRIMARY KEY (effective_dt, production_set_id, user_id)
);
CREATE TABLE extra_schedule_event
(
    effective_dt DATE NOT NULL,
    production_set_id VARCHAR(50) NOT NULL,
    title VARCHAR(100) NOT NULL,
    description VARCHAR(500) NOT NULL,
    start_ts TIMESTAMP NOT NULL,
    end_ts TIMESTAMP NOT NULL,
    scene_desc EXTRA_SCHEDULE_EVENT_SCENE_DESC DEFAULT 'INTERIOR'::extra_schedule_event_scene_desc NOT NULL,
    time_of_day EXTRA_SCHEDULE_EVENT_TIME_OF_DAY DEFAULT 'DAY'::extra_schedule_event_time_of_day NOT NULL,
    updated_ts TIMESTAMP WITH TIME ZONE DEFAULT now() NOT NULL,
    created_ts TIMESTAMP WITH TIME ZONE DEFAULT now() NOT NULL,
    CONSTRAINT extra_schedule_event_effective_dt_production_set_id_title_start PRIMARY KEY (effective_dt, production_set_id, title, start_ts)
);
CREATE TABLE extra_schedule_event_item
(
    effective_dt DATE NOT NULL,
    production_set_id VARCHAR(100) NOT NULL,
    user_id TEXT NOT NULL,
    title VARCHAR(100) NOT NULL,
    start_ts TIMESTAMP NOT NULL,
    CONSTRAINT extra_schedule_event_item_effective_dt_production_set_id_user_i PRIMARY KEY (effective_dt, production_set_id, user_id, title, start_ts)
);
CREATE TABLE extra_wardrobe_status
(
    user_id VARCHAR(100) NOT NULL,
    effective_dt DATE NOT NULL,
    production_set_id VARCHAR(100) NOT NULL,
    checkin_ts TIMESTAMP,
    checkout_ts TIMESTAMP,
    CONSTRAINT extra_wardrobe_status_user_id_effective_dt_production_set_id_pk PRIMARY KEY (user_id, effective_dt, production_set_id)
);
CREATE TABLE login
(
    user_id VARCHAR(100) PRIMARY KEY NOT NULL,
    password_salt VARCHAR(100) NOT NULL
);
CREATE TABLE meal
(
    effective_dt DATE NOT NULL,
    production_set_id VARCHAR(50) NOT NULL,
    start_ts TIMESTAMP NOT NULL,
    duration INTEGER NOT NULL,
    created_ts TIMESTAMP WITH TIME ZONE DEFAULT now() NOT NULL,
    updated_ts TIMESTAMP WITH TIME ZONE DEFAULT now() NOT NULL,
    CONSTRAINT meal_effective_dt_production_set_id_start_ts_pk PRIMARY KEY (effective_dt, production_set_id, start_ts)
);
CREATE TABLE paper_form
(
    paper_form_id TEXT PRIMARY KEY NOT NULL,
    paper_form_fields_id INTEGER,
    created_ts TIMESTAMP WITH TIME ZONE NOT NULL,
    updated_ts TIMESTAMP WITH TIME ZONE NOT NULL,
    due_dt DATE,
    user_filler_id TEXT,
    user_verifier_id TEXT,
    signed_ts TIMESTAMP WITH TIME ZONE
);
CREATE TABLE paper_form_field
(
    paper_form_id TEXT NOT NULL,
    paper_form_template_field_id TEXT NOT NULL,
    value TEXT,
    created_ts TIMESTAMP WITH TIME ZONE NOT NULL,
    updated_ts TIMESTAMP WITH TIME ZONE NOT NULL,
    CONSTRAINT paper_form_field_paper_form_id_paper_form_template_field_id_pk PRIMARY KEY (paper_form_id, paper_form_template_field_id)
);
CREATE TABLE paper_form_template
(
    paper_form_template_id TEXT PRIMARY KEY NOT NULL,
    base_image64 BYTEA,
    created_ts TIMESTAMP WITH TIME ZONE DEFAULT now() NOT NULL,
    updated_ts TIMESTAMP WITH TIME ZONE DEFAULT now() NOT NULL
);
CREATE TABLE paper_form_template_field
(
    paper_form_template_field_id TEXT PRIMARY KEY NOT NULL,
    label TEXT NOT NULL,
    owner TEXT NOT NULL,
    coord_x INTEGER,
    coord_y INTEGER,
    paper_form_template_id TEXT NOT NULL
);
CREATE TABLE production
(
    production_id VARCHAR(100) PRIMARY KEY NOT NULL
);
CREATE TABLE production_set
(
    production_set_id VARCHAR(100) PRIMARY KEY NOT NULL,
    start_dt DATE NOT NULL,
    end_dt DATE,
    location_addr VARCHAR(100),
    production_id VARCHAR(100) NOT NULL
);
CREATE TABLE profile_field
(
    profile_field_id TEXT PRIMARY KEY NOT NULL,
    label TEXT NOT NULL
);
CREATE TABLE profile_field_input
(
    profile_field_id TEXT,
    input TEXT,
    updated_ts TIMESTAMP WITH TIME ZONE NOT NULL,
    profile_field_input_id INTEGER DEFAULT nextval('profile_field_input_profile_field_input_id_seq'::regclass) PRIMARY KEY NOT NULL,
    user_id TEXT
);
CREATE TABLE profile_field_paper_template_form_field
(
    profile_field_id TEXT NOT NULL,
    paper_form_template_field_id TEXT NOT NULL
);
CREATE TABLE skin
(
    effective_dt DATE NOT NULL,
    production_set_id VARCHAR(100) NOT NULL,
    created_ts TIMESTAMP WITH TIME ZONE DEFAULT now() NOT NULL,
    updated_ts TIMESTAMP WITH TIME ZONE DEFAULT now() NOT NULL,
    CONSTRAINT skin_effectivedt_production_set_id_pk PRIMARY KEY (effective_dt, production_set_id)
);
CREATE TABLE skin_item
(
    effective_dt DATE NOT NULL,
    production_set_id TEXT NOT NULL,
    email TEXT NOT NULL,
    call_start_ts TIMESTAMP,
    role VARCHAR(50),
    full_name VARCHAR(100),
    extra_talent_type VARCHAR(50),
    notes TEXT,
    rate VARCHAR(20),
    CONSTRAINT skin_item_effective_dt_production_set_id_extra_id_pk PRIMARY KEY (effective_dt, production_set_id, email)
);
CREATE TABLE "user"
(
    user_id VARCHAR(100) PRIMARY KEY NOT NULL,
    created_ts TIMESTAMP WITH TIME ZONE DEFAULT now() NOT NULL,
    updated_ts TIMESTAMP WITH TIME ZONE DEFAULT now() NOT NULL
);
CREATE TABLE user_profile
(
    user_id TEXT PRIMARY KEY NOT NULL,
    last_submitted_ts TIMESTAMP WITH TIME ZONE,
    s3_icon_avatar_url VARCHAR(500)
);
ALTER TABLE extra ADD FOREIGN KEY (user_id) REFERENCES "user" (user_id);
CREATE UNIQUE INDEX extra_user_id_uindex ON extra (user_id);
ALTER TABLE extra_daily_activity ADD FOREIGN KEY (user_id) REFERENCES extra (user_id);
ALTER TABLE extra_daily_activity ADD FOREIGN KEY (effective_dt, production_set_id, meal_start_ts) REFERENCES meal (effective_dt, production_set_id, start_ts);
ALTER TABLE extra_schedule ADD FOREIGN KEY (user_id) REFERENCES extra (user_id);
ALTER TABLE extra_schedule_event_item ADD FOREIGN KEY (effective_dt, production_set_id, user_id) REFERENCES extra_schedule (effective_dt, production_set_id, user_id);
ALTER TABLE extra_schedule_event_item ADD FOREIGN KEY (effective_dt, production_set_id, title, start_ts) REFERENCES extra_schedule_event (effective_dt, production_set_id, title, start_ts);
ALTER TABLE extra_wardrobe_status ADD FOREIGN KEY (user_id) REFERENCES extra (user_id);
CREATE UNIQUE INDEX login_user_id_uindex ON login (user_id);
ALTER TABLE meal ADD FOREIGN KEY (production_set_id) REFERENCES production_set (production_set_id);
ALTER TABLE paper_form ADD FOREIGN KEY (user_filler_id) REFERENCES "user" (user_id);
ALTER TABLE paper_form ADD FOREIGN KEY (user_verifier_id) REFERENCES "user" (user_id);
ALTER TABLE paper_form ADD FOREIGN KEY (user_verifier_id) REFERENCES "user" (user_id);
CREATE UNIQUE INDEX paper_form_paper_form_id_uindex ON paper_form (paper_form_id);
ALTER TABLE paper_form_field ADD FOREIGN KEY (paper_form_id) REFERENCES paper_form (paper_form_id);
ALTER TABLE paper_form_field ADD FOREIGN KEY (paper_form_template_field_id) REFERENCES paper_form_template_field (paper_form_template_field_id);
CREATE UNIQUE INDEX paper_form_template_paper_form_template_id_uindex ON paper_form_template (paper_form_template_id);
ALTER TABLE paper_form_template_field ADD FOREIGN KEY (paper_form_template_id) REFERENCES paper_form_template (paper_form_template_id);
CREATE UNIQUE INDEX paper_form_template_field_paper_form_template_field_id_uindex ON paper_form_template_field (paper_form_template_field_id);
CREATE UNIQUE INDEX production_production_id_uindex ON production (production_id);
ALTER TABLE production_set ADD FOREIGN KEY (production_id) REFERENCES production (production_id);
CREATE UNIQUE INDEX production_set_production_set_id_uindex ON production_set (production_set_id);
CREATE UNIQUE INDEX profile_field_profile_field_id_uindex ON profile_field (profile_field_id);
ALTER TABLE profile_field_input ADD FOREIGN KEY (profile_field_id) REFERENCES profile_field (profile_field_id);
ALTER TABLE profile_field_input ADD FOREIGN KEY (user_id) REFERENCES user_profile (user_id);
CREATE UNIQUE INDEX profile_field_input_profile_field_input_id_uindex ON profile_field_input (profile_field_input_id);
ALTER TABLE profile_field_paper_template_form_field ADD FOREIGN KEY (profile_field_id) REFERENCES profile_field (profile_field_id);
ALTER TABLE profile_field_paper_template_form_field ADD FOREIGN KEY (paper_form_template_field_id) REFERENCES paper_form_template_field (paper_form_template_field_id);
CREATE UNIQUE INDEX profile_field_paper_template_form_field_profile_field_id_uindex ON profile_field_paper_template_form_field (profile_field_id);
ALTER TABLE skin_item ADD FOREIGN KEY (effective_dt, production_set_id) REFERENCES skin (effective_dt, production_set_id);
ALTER TABLE skin_item ADD FOREIGN KEY (email) REFERENCES extra (user_id);
CREATE UNIQUE INDEX user_user_id_uindex ON "user" (user_id);
ALTER TABLE user_profile ADD FOREIGN KEY (user_id) REFERENCES "user" (user_id);
CREATE UNIQUE INDEX user_profile_user_id_uindex ON user_profile (user_id);
