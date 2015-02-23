# core model types
# --- !Ups

CREATE SEQUENCE subscriber_id_seq;
CREATE TABLE subscriber (
  id integer NOT NULL DEFAULT nextval('subscriber_id_seq'),
  hub_user_id integer,
  name varchar,
  category varchar,
  contact varchar,
  link varchar,
  logo varchar,
  created timestamp,
  FOREIGN KEY(hub_user_id) REFERENCES hub_user(id),
  PRIMARY KEY(id)
);

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


# --- !Downs

DROP TABLE interest;
DROP SEQUENCE interest_id_seq;
DROP TABLE subscriber;
DROP SEQUENCE subscriber_id_seq;
