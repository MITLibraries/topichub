# core model types
# --- !Ups

DROP TABLE interest;
DROP SEQUENCE interest_id_seq;

CREATE SEQUENCE interest_id_seq;
CREATE TABLE interest (
  id integer NOT NULL DEFAULT nextval('interest_id_seq'),
  subscriber_id integer,
  scheme_tag varchar,
  int_value varchar,
  template boolean,
  created timestamp,
  FOREIGN KEY(subscriber_id) REFERENCES subscriber(id),
  PRIMARY KEY(id)
);

# --- !Downs

DROP TABLE interest;
DROP SEQUENCE interest_id_seq;

CREATE SEQUENCE interest_id_seq;
CREATE TABLE interest (
  id integer NOT NULL DEFAULT nextval('interest_id_seq'),
  subscriber_id integer,
  scheme_id integer,
  action varchar,
  created timestamp,
  FOREIGN KEY(subscriber_id) REFERENCES subscriber(id),
  FOREIGN KEY(scheme_id) REFERENCES scheme(id),
  PRIMARY KEY(id)
);
