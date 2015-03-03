# core model types
# --- !Ups

CREATE SEQUENCE subscription_id_seq;
CREATE TABLE subscription (
  id integer NOT NULL DEFAULT nextval('subscription_id_seq'),
  subscriber_id integer,
  topic_id integer,
  action varchar,
  created timestamp,
  updated timestamp,
  cancelled timestamp,
  earliest timestamp,
  latest timestamp,
  active boolean,
  FOREIGN KEY(subscriber_id) REFERENCES subscriber(id),
  FOREIGN KEY(topic_id) REFERENCES topic(id),
  PRIMARY KEY(id)
);

CREATE SEQUENCE hold_id_seq;
CREATE TABLE hold (
  id integer NOT NULL DEFAULT nextval('hold_id_seq'),
  subscriber_id integer,
  subscription_id integer,
  item_id integer,
  created timestamp,
  released timestamp,
  FOREIGN KEY(subscriber_id) REFERENCES subscriber(id),
  FOREIGN KEY(subscription_id) REFERENCES subscription(id),
  FOREIGN KEY(item_id) REFERENCES item(id),
  PRIMARY KEY(id)
);

CREATE SEQUENCE transfer_id_seq;
CREATE TABLE transfer (
  id integer NOT NULL DEFAULT nextval('hold_id_seq'),
  subscriber_id integer,
  subscription_id integer,
  item_id integer,
  action varchar,
  created timestamp,
  FOREIGN KEY(subscriber_id) REFERENCES subscriber(id),
  FOREIGN KEY(subscription_id) REFERENCES subscription(id),
  FOREIGN KEY(item_id) REFERENCES item(id),
  PRIMARY KEY(id)
);

# --- !Downs

DROP TABLE subscriber;
DROP SEQUENCE subscriber_id_seq;
DROP TABLE hold;
DROP SEQUENCE hold_id_seq;
DROP TABLE transfer;
DROP SEQUENCE transfer_id_seq;
