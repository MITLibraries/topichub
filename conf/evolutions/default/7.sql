# --- !Ups

ALTER TABLE hub_user ADD COLUMN identity varchar;
ALTER TABLE hub_user DROP COLUMN password;

# --- !Downs

ALTER TABLE hub_user DROP COLUMN identity;
ALTER TABLE hub_user ADD COLUMN password varchar;
