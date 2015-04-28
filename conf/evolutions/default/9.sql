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

CREATE SEQUENCE interest_subscription_id_seq;
CREATE TABLE interest_subscription (
  id integer NOT NULL DEFAULT nextval('interest_subscription_id_seq'),
  interest_id integer,
  subscription_id integer,
  FOREIGN KEY(interest_id) REFERENCES interest(id),
  FOREIGN KEY(subscription_id) REFERENCES subscription(id),
  PRIMARY KEY(id)
);

# --- !Downs

DROP TABLE interest_subscription;
DROP SEQUENCE interest_subscription_id_seq;
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
