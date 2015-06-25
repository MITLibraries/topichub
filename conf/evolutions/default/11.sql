# --- !Ups

ALTER TABLE collection ADD COLUMN active boolean DEFAULT true;

# --- !Downs

ALTER TABLE collection DROP COLUMN active;
