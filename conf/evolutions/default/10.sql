# core model types
# --- !Ups
CREATE SEQUENCE hub_user_subscriber_id_seq;
CREATE TABLE hub_user_subscriber (
  id integer NOT NULL DEFAULT nextval('hub_user_subscriber_id_seq'),
  hub_user_id integer NOT NULL,
  subscriber_id integer NOT NULL,
  admin boolean default false,
  approved boolean default false,
  updated timestamp default current_timestamp,
  UNIQUE(hub_user_id, subscriber_id),
  FOREIGN KEY(subscriber_id) REFERENCES subscriber(id),
  FOREIGN KEY(hub_user_id) REFERENCES hub_user(id),
  PRIMARY KEY(id)
);
ALTER TABLE subscriber DROP COLUMN hub_user_id;

# --- !Downs
DROP TABLE hub_user_subscriber;
DROP SEQUENCE hub_user_subscriber_id_seq;
ALTER TABLE subscriber ADD COLUMN hub_user_id integer;
