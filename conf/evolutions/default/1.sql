# core model types
# --- !Ups

CREATE SEQUENCE hub_user_id_seq;
CREATE TABLE hub_user (
  id integer NOT NULL DEFAULT nextval('hub_user_id_seq'),
  name varchar UNIQUE,
  email varchar,
  password varchar,
  role varchar,
  created timestamp,
  accessed timestamp,
  PRIMARY KEY(id)
);

CREATE SEQUENCE publisher_id_seq;
CREATE TABLE publisher (
  id integer NOT NULL DEFAULT nextval('publisher_id_seq'),
  hub_user_id integer,
  tag varchar UNIQUE,
  name varchar,
  description varchar,
  category varchar,
  status varchar,
  link varchar,
  logo varchar,
  source varchar,
  created timestamp,
  FOREIGN KEY(hub_user_id) REFERENCES hub_user(id),
  PRIMARY KEY(id)
);

CREATE SEQUENCE harvest_id_seq;
CREATE TABLE harvest (
  id integer NOT NULL DEFAULT nextval('harvest_id_seq'),
  publisher_id integer,
  name varchar UNIQUE,
  protocol varchar,
  service_url varchar,
  resource_url varchar,
  freq integer,
  start timestamp,
  updated timestamp,
  FOREIGN KEY(publisher_id) REFERENCES publisher(id),
  PRIMARY KEY(id)
);

CREATE SEQUENCE content_type_id_seq;
CREATE TABLE content_type (
  id integer NOT NULL DEFAULT nextval('content_type_id_seq'),
  tag varchar UNIQUE,
  label varchar,
  description varchar,
  logo varchar,
  PRIMARY KEY(id)
);

CREATE SEQUENCE content_format_id_seq;
CREATE TABLE content_format (
  id integer NOT NULL DEFAULT nextval('content_format_id_seq'),
  tag varchar UNIQUE,
  label varchar,
  description varchar,
  url varchar,
  mimetype varchar,
  logo varchar,
  PRIMARY KEY(id)
);

CREATE SEQUENCE resource_map_id_seq;
CREATE TABLE resource_map (
  id integer NOT NULL DEFAULT nextval('resource_map_id_seq'),
  tag varchar UNIQUE,
  description varchar,
  sword_url varchar,
  PRIMARY KEY(id)
);

CREATE SEQUENCE collection_id_seq;
CREATE TABLE collection (
  id integer NOT NULL DEFAULT nextval('collection_id_seq'),
  publisher_id integer,
  content_type_id integer,
  resource_map_id integer,
  tag varchar UNIQUE,
  description varchar,
  policy varchar,
  created timestamp,
  updated timestamp,
  deposits integer,
  FOREIGN KEY(publisher_id) REFERENCES publisher(id),
  FOREIGN KEY(content_type_id) REFERENCES content_type(id),
  FOREIGN KEY(resource_map_id) REFERENCES resource_map(id),
  PRIMARY KEY(id)
);

CREATE SEQUENCE scheme_id_seq;
CREATE TABLE scheme (
  id integer NOT NULL DEFAULT nextval('scheme_id_seq'),
  tag varchar(255) UNIQUE,
  gentype varchar,
  category varchar,
  description varchar,
  link varchar,
  logo varchar,
  created timestamp,
  PRIMARY KEY(id)
);

CREATE SEQUENCE content_type_scheme_id_seq;
CREATE TABLE content_type_scheme (
  id integer NOT NULL DEFAULT nextval('content_type_scheme_id_seq'),
  content_type_id integer,
  scheme_id integer,
  relation varchar,
  FOREIGN KEY(content_type_id) REFERENCES content_type(id),
  FOREIGN KEY(scheme_id) REFERENCES scheme(id),
  PRIMARY KEY(id)
);

CREATE SEQUENCE resource_map_scheme_id_seq;
CREATE TABLE resource_map_scheme (
  id integer NOT NULL DEFAULT nextval('resource_map_scheme_id_seq'),
  resource_map_id integer,
  scheme_id integer,
  content_format_id integer,
  source varchar(255) NOT NULL,
  rank integer,
  FOREIGN KEY(resource_map_id) REFERENCES resource_map(id),
  FOREIGN KEY(scheme_id) REFERENCES scheme(id),
  FOREIGN KEY(content_format_id) REFERENCES content_format(id),
  PRIMARY KEY(id)
);

CREATE SEQUENCE topic_id_seq;
CREATE TABLE topic (
  id integer NOT NULL DEFAULT nextval('topic_id_seq'),
  scheme_id integer,
  tag varchar(255),
  name varchar,
  link varchar,
  created timestamp,
  updated timestamp,
  transfers integer,
  FOREIGN KEY(scheme_id) REFERENCES scheme(id),
  PRIMARY KEY(id)
);

CREATE SEQUENCE item_id_seq;
CREATE TABLE item (
  id integer NOT NULL DEFAULT nextval('item_id_seq'),
  collection_id integer,
  content_type_id integer,
  location varchar NOT NULL,
  obj_key varchar(255) UNIQUE,
  created timestamp,
  updated timestamp,
  transfers integer,
  FOREIGN KEY(collection_id) REFERENCES collection(id),
  FOREIGN KEY(content_type_id) REFERENCES content_type(id),
  PRIMARY KEY(id)
);

CREATE SEQUENCE metadata_id_seq;
CREATE TABLE metadata (
  id integer NOT NULL DEFAULT nextval('metadata_id_seq'),
  item_id integer,
  mdname varchar,
  mdvalue varchar,
  FOREIGN KEY(item_id) REFERENCES item(id),
  PRIMARY KEY(id)
);

CREATE SEQUENCE item_topic_id_seq;
CREATE TABLE item_topic (
  id integer NOT NULL DEFAULT nextval('item_topic_id_seq'),
  item_id integer,
  item_created timestamp,
  topic_id integer,
  FOREIGN KEY(item_id) REFERENCES item(id),
  FOREIGN KEY(topic_id) REFERENCES topic(id),
  PRIMARY KEY(id)
);

CREATE SEQUENCE finder_id_seq;
CREATE TABLE finder (
  id integer NOT NULL DEFAULT nextval('finder_id_seq'),
  scheme_id integer,
  content_format_id integer,
  description varchar(255) NOT NULL,
  cardinality varchar(255) NOT NULL,
  id_key varchar(255) NOT NULL,
  id_label varchar(255) NOT NULL,
  author varchar,
  created timestamp,
  FOREIGN KEY(scheme_id) REFERENCES scheme(id),
  FOREIGN KEY(content_format_id) REFERENCES content_format(id),
  PRIMARY KEY (id)
);

CREATE SEQUENCE validator_id_seq;
CREATE TABLE validator (
  id integer NOT NULL DEFAULT nextval('validator_id_seq'),
  scheme_id integer,
  description varchar(255) NOT NULL,
  user_id varchar,
  password varchar,
  service_code varchar,
  service_url varchar,
  author varchar,
  created timestamp,
  FOREIGN KEY(scheme_id) REFERENCES scheme(id),
  PRIMARY KEY (id)
);

# --- !Downs

DROP TABLE metadata;
DROP SEQUENCE metadata_id_seq;
DROP TABLE finder;
DROP SEQUENCE finder_id_seq;
DROP TABLE validator;
DROP SEQUENCE validator_id_seq;
DROP TABLE item_topic;
DROP SEQUENCE item_topic_id_seq;
DROP TABLE topic;
DROP SEQUENCE topic_id_seq;
DROP TABLE content_type_scheme;
DROP SEQUENCE content_type_scheme_id_seq;
DROP TABLE resource_map_scheme;
DROP SEQUENCE resource_map_scheme_id_seq;
DROP TABLE scheme;
DROP SEQUENCE scheme_id_seq;
DROP TABLE item;
DROP SEQUENCE item_id_seq;
DROP TABLE collection;
DROP SEQUENCE collection_id_seq;
DROP TABLE harvest;
DROP SEQUENCE harvest_id_seq;
DROP TABLE content_type;
DROP SEQUENCE content_type_id_seq;
DROP TABLE resource_map;
DROP SEQUENCE resource_map_id_seq;
DROP TABLE content_format;
DROP SEQUENCE content_format_id_seq;
DROP TABLE publisher;
DROP SEQUENCE publisher_id_seq;
DROP TABLE hub_user;
DROP SEQUENCE hub_user_id_seq;
