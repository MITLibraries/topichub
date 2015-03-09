# core model types
# --- !Ups

CREATE SEQUENCE content_profile_id_seq;
CREATE TABLE content_profile (
  id integer NOT NULL DEFAULT nextval('content_profile_id_seq'),
  tag varchar UNIQUE,
  label varchar,
  description varchar,
  PRIMARY KEY(id)
);

CREATE SEQUENCE content_profile_scheme_id_seq;
CREATE TABLE content_profile_scheme (
  id integer NOT NULL DEFAULT nextval('content_profile_scheme_id_seq'),
  content_profile_id integer,
  scheme_id integer,
  FOREIGN KEY(content_profile_id) REFERENCES content_profile(id),
  FOREIGN KEY(scheme_id) REFERENCES scheme(id),
  PRIMARY KEY(id)
);

# --- !Downs

DROP TABLE content_profile;
DROP SEQUENCE content_profile_id_seq;
DROP TABLE content_profile_scheme;
DROP SEQUENCE content_profile_scheme_id_seq;
