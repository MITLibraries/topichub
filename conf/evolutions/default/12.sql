# --- !Ups

CREATE SEQUENCE cull_id_seq;
CREATE TABLE cull (
  id integer NOT NULL DEFAULT nextval('cull_id_seq'),
  publisher_id integer,
  name varchar UNIQUE,
  policy varchar,
  notify_url varchar,
  freq integer,
  start timestamp,
  updated timestamp,
  FOREIGN KEY(publisher_id) REFERENCES publisher(id),
  PRIMARY KEY(id)
);

# --- !Downs

DROP TABLE cull;
DROP SEQUENCE cull_id_seq;
