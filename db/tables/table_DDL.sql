CREATE TABLE extra
(
    user_id TEXT PRIMARY KEY NOT NULL,
    CONSTRAINT extra_user_user_id_fk FOREIGN KEY (user_id) REFERENCES "user" (user_id)
);
CREATE UNIQUE INDEX extra_user_id_uindex ON extra (user_id);
CREATE TABLE extra_schedule
(
    effective_dt DATE NOT NULL,
    production_set_id VARCHAR(50) NOT NULL,
    created_ts TIMESTAMP WITH TIME ZONE DEFAULT now() NOT NULL,
    updated_ts TIMESTAMP WITH TIME ZONE DEFAULT now() NOT NULL,
    user_id TEXT NOT NULL,
    CONSTRAINT extra_schedule_effective_dt_production_shoot_lat_production_sho PRIMARY KEY (effective_dt, production_set_id, user_id),
    CONSTRAINT extra_schedule_production_set_production_set_id_fk FOREIGN KEY (production_set_id) REFERENCES production_set (production_set_id),
    CONSTRAINT extra_schedule_extra_user_id_fk FOREIGN KEY (user_id) REFERENCES extra (user_id)
);
CREATE TABLE extra_task
(
    effective_dt DATE NOT NULL,
    production_set_id VARCHAR(50) NOT NULL,
    user_id TEXT NOT NULL,
    title VARCHAR(100) NOT NULL,
    description VARCHAR(500) NOT NULL,
    start_ts TIMESTAMP NOT NULL,
    end_ts TIMESTAMP NOT NULL,
    created_ts TIMESTAMP WITH TIME ZONE DEFAULT now() NOT NULL,
    updated_ts TIMESTAMP WITH TIME ZONE DEFAULT now() NOT NULL,
    CONSTRAINT extra_task_effective_dt_production_set_id_user_id_pk PRIMARY KEY (effective_dt, production_set_id, user_id),
    CONSTRAINT extra_task_extra_schedule_user_id_effective_dt_production_set_i FOREIGN KEY (user_id, effective_dt, production_set_id) REFERENCES extra_schedule (effective_dt, production_set_id, user_id)
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
    signed_ts TIMESTAMP WITH TIME ZONE,
    CONSTRAINT paper_form_user_filler_id_fk FOREIGN KEY (user_filler_id) REFERENCES "user" (user_id),
    CONSTRAINT paper_form_user_verifier_id_fk FOREIGN KEY (user_verifier_id) REFERENCES "user" (user_id)
);
CREATE UNIQUE INDEX paper_form_paper_form_id_uindex ON paper_form (paper_form_id);
CREATE TABLE paper_form_field
(
    paper_form_id TEXT NOT NULL,
    paper_form_template_field_id TEXT NOT NULL,
    value TEXT,
    created_ts TIMESTAMP WITH TIME ZONE NOT NULL,
    updated_ts TIMESTAMP WITH TIME ZONE NOT NULL,
    CONSTRAINT paper_form_field_paper_form_id_paper_form_template_field_id_pk PRIMARY KEY (paper_form_id, paper_form_template_field_id),
    CONSTRAINT paper_form_field_paper_form_paper_form_id_fk FOREIGN KEY (paper_form_id) REFERENCES paper_form (paper_form_id),
    CONSTRAINT paper_form_field_paper_form_template_field_paper_form_template_ FOREIGN KEY (paper_form_template_field_id) REFERENCES paper_form_template_field (paper_form_template_field_id)
);
CREATE TABLE paper_form_template
(
    paper_form_template_id TEXT PRIMARY KEY NOT NULL,
    base_image64 BYTEA,
    created_ts TIMESTAMP WITH TIME ZONE DEFAULT now() NOT NULL,
    updated_ts TIMESTAMP WITH TIME ZONE DEFAULT now() NOT NULL
);
CREATE UNIQUE INDEX paper_form_template_paper_form_template_id_uindex ON paper_form_template (paper_form_template_id);
CREATE TABLE paper_form_template_field
(
    paper_form_template_field_id TEXT PRIMARY KEY NOT NULL,
    label TEXT NOT NULL,
    owner TEXT NOT NULL,
    coord_x INTEGER,
    coord_y INTEGER,
    paper_form_template_id TEXT NOT NULL,
    CONSTRAINT paper_form_template_field_paper_form_template_paper_form_templa FOREIGN KEY (paper_form_template_id) REFERENCES paper_form_template (paper_form_template_id)
);
CREATE UNIQUE INDEX paper_form_template_field_paper_form_template_field_id_uindex ON paper_form_template_field (paper_form_template_field_id);
CREATE TABLE production
(
    production_id VARCHAR(100) PRIMARY KEY NOT NULL
);
CREATE UNIQUE INDEX production_production_id_uindex ON production (production_id);
CREATE TABLE production_set
(
    production_set_id VARCHAR(100) PRIMARY KEY NOT NULL,
    start_dt DATE NOT NULL,
    end_dt DATE,
    location_addr VARCHAR(100),
    production_id VARCHAR(100) NOT NULL,
    CONSTRAINT production_set_production_production_id_fk FOREIGN KEY (production_id) REFERENCES production (production_id)
);
CREATE UNIQUE INDEX production_set_production_set_id_uindex ON production_set (production_set_id);
CREATE TABLE profile_field
(
    profile_field_id TEXT PRIMARY KEY NOT NULL,
    label TEXT NOT NULL
);
CREATE UNIQUE INDEX profile_field_profile_field_id_uindex ON profile_field (profile_field_id);
CREATE TABLE profile_field_input
(
    profile_field_id TEXT,
    input TEXT,
    updated_ts TIMESTAMP WITH TIME ZONE NOT NULL,
    profile_field_input_id INTEGER DEFAULT nextval('profile_field_input_profile_field_input_id_seq'::regclass) PRIMARY KEY NOT NULL,
    user_id TEXT,
    CONSTRAINT profile_field_input_profile_field_profile_field_id_fk FOREIGN KEY (profile_field_id) REFERENCES profile_field (profile_field_id),
    CONSTRAINT profile_field_input_user_profile_user_id_fk FOREIGN KEY (user_id) REFERENCES user_profile (user_id)
);
CREATE UNIQUE INDEX profile_field_input_profile_field_input_id_uindex ON profile_field_input (profile_field_input_id);
CREATE TABLE profile_field_paper_template_form_field
(
    profile_field_id TEXT NOT NULL,
    paper_form_template_field_id TEXT NOT NULL,
    CONSTRAINT profile_field_paper_template_form_field_profile_field_profile_f FOREIGN KEY (profile_field_id) REFERENCES profile_field (profile_field_id),
    CONSTRAINT profile_field_paper_template_form_field_paper_form_template_fie FOREIGN KEY (paper_form_template_field_id) REFERENCES paper_form_template_field (paper_form_template_field_id)
);
CREATE UNIQUE INDEX profile_field_paper_template_form_field_profile_field_id_uindex ON profile_field_paper_template_form_field (profile_field_id);
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
    first_name VARCHAR(75) NOT NULL,
    last_name VARCHAR(75) NOT NULL,
    call_start_ts TIMESTAMP,
    role VARCHAR(50),
    pay VARCHAR(50),
    CONSTRAINT skin_item_effective_dt_production_set_id_extra_id_pk PRIMARY KEY (effective_dt, production_set_id, email),
    CONSTRAINT skin_item_skin_effective_dt_production_set_id_fk FOREIGN KEY (effective_dt, production_set_id) REFERENCES skin (effective_dt, production_set_id)
);
CREATE TABLE "user"
(
    user_id VARCHAR(100) PRIMARY KEY NOT NULL,
    password_salt VARCHAR(100) NOT NULL
);
CREATE UNIQUE INDEX user_user_id_uindex ON "user" (user_id);
CREATE TABLE user_profile
(
    user_id TEXT NOT NULL,
    last_submitted_ts TIMESTAMP WITH TIME ZONE,
    s3_icon_avatar_url VARCHAR(500),
    CONSTRAINT user_profile_user_user_id_fk FOREIGN KEY (user_id) REFERENCES "user" (user_id)
);
CREATE UNIQUE INDEX user_profile_user_id_uindex ON user_profile (user_id);
