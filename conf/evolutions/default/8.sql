# core model types
# --- !Ups

CREATE SEQUENCE plan_id_seq;
CREATE TABLE plan (
  id integer NOT NULL DEFAULT nextval('plan_id_seq'),
  subscriber_id integer,
  channel_id integer,
  name varchar,
  description varchar,
  icon varchar,
  fulfill varchar,
  pick varchar,
  interest varchar,
  template varchar,
  created timestamp,
  FOREIGN KEY(subscriber_id) REFERENCES subscriber(id),
  FOREIGN KEY(channel_id) REFERENCES channel(id),
  PRIMARY KEY(id)
);

CREATE SEQUENCE plan_scheme_id_seq;
CREATE TABLE plan_scheme (
  id integer NOT NULL DEFAULT nextval('plan_scheme_id_seq'),
  plan_id integer,
  scheme_id integer,
  created timestamp,
  FOREIGN KEY(plan_id) REFERENCES plan(id),
  FOREIGN KEY(scheme_id) REFERENCES scheme(id),
  PRIMARY KEY(id)
);

# --- !Downs

DROP TABLE plan_scheme;
DROP SEQUENCE plan_scheme_id_seq;
DROP TABLE plan
DROP SEQUENCE plan_id_seq;
