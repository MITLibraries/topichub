# --- !Ups
CREATE INDEX interest_subscriber_id_idx ON interest (subscriber_id);
CREATE INDEX interest_scheme_tag_idx ON interest (scheme_tag);
CREATE INDEX interest_subscription_interest_id_idx ON interest_subscription (interest_id);
CREATE INDEX item_obj_key_idx ON item (obj_key);
CREATE INDEX item_obj_created ON item (created);
CREATE INDEX item_topic_item_id_idx ON item_topic (item_id);
CREATE INDEX item_topic_topic_id_idx ON item_topic (topic_id);
CREATE INDEX metadata_mdname_idx ON metadata (mdname);
CREATE INDEX metadata_mdvalue_idx ON metadata (mdvalue);
CREATE INDEX plan_scheme_plan_id_idx ON plan_scheme (plan_id);
CREATE INDEX plan_scheme_scheme_id_idx ON plan_scheme (scheme_id);
CREATE INDEX subscription_topic_id_idx ON subscription (topic_id);
CREATE INDEX topic_tag_idx ON topic (tag);
CREATE INDEX topic_scheme_id_idx ON topic (scheme_id);
CREATE INDEX topic_pick_topic_id_idx ON topic_pick (topic_id);
CREATE INDEX topic_pick_subscriber_id_idx ON topic_pick (subscriber_id);

# --- !Downs
DROP INDEX interest_subscriber_id_idx;
DROP INDEX interest_subscription_interest_id_idx;
DROP INDEX item_obj_key_idx;
DROP INDEX item_obj_created;
DROP INDEX item_topic_item_id_idx;
DROP INDEX item_topic_topic_id_idx;
DROP INDEX metadata_mdname_idx;
DROP INDEX metadata_mdvalue_idx;
DROP INDEX plan_scheme_plan_id_idx;
DROP INDEX plan_scheme_scheme_id_idx;
DROP INDEX subscription_topic_id_idx;
DROP INDEX topic_tag_idx;
DROP INDEX topic_scheme_id_idx;
DROP INDEX topic_pick_topic_id_idx;
DROP INDEX topic_pick_subscriber_id_idx;
