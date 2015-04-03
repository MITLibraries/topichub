# core model types
# --- !Ups

-- Fixes #93 tag field too short: varchar(255)
ALTER TABLE topic ALTER tag TYPE varchar;

CREATE SEQUENCE agent_id_seq;
CREATE TABLE agent (
  id integer NOT NULL DEFAULT nextval('agent_id_seq'),
  tag varchar UNIQUE,
  label varchar,
  description varchar,
  code varchar,
  params varchar,
  icon varchar,
  PRIMARY KEY(id)
);

CREATE SEQUENCE topic_pick_id_seq;
CREATE TABLE topic_pick (
  id integer NOT NULL DEFAULT nextval('topic_pick_id_seq'),
  subscriber_id integer,
  topic_id integer,
  agent_id integer,
  created timestamp,
  resolved timestamp,
  FOREIGN KEY(subscriber_id) REFERENCES subscriber(id),
  FOREIGN KEY(topic_id) REFERENCES topic(id),
  FOREIGN KEY(agent_id) REFERENCES agent(id),
  PRIMARY KEY(id)
);

# --- !Downs

ALTER TABLE topic ALTER tag TYPE varchar(255);

DROP TABLE topic_pick;
DROP SEQUENCE topic_pick_id_seq;
DROP TABLE agent;
DROP SEQUENCE agent_id_seq;
