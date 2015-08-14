# --- !Ups

ALTER TABLE topic_pick ADD COLUMN basis varchar;

# --- !Downs

ALTER TABLE topic_pick DROP COLUMN basis;
