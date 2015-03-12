# core model types
# --- !Ups

CREATE SEQUENCE channel_id_seq;
CREATE TABLE channel (
  id integer NOT NULL DEFAULT nextval('channel_id_seq'),
  subscriber_id integer,
  protocol varchar NOT NULL,
  mode varchar NOT NULL,
  description varchar NOT NULL,
  user_id varchar(255) NOT NULL,
  password varchar(255) NOT NULL,
  channel_url varchar(255) NOT NULL,
  created timestamp,
  updated timestamp,
  transfers integer,
  FOREIGN KEY(subscriber_id) REFERENCES subscriber(id),
  PRIMARY KEY (id)
);

# --- !Downs

DROP TABLE channel;
DROP SEQUENCE channel_id_seq;
