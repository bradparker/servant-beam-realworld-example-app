-- Revert conduit:create-tags from pg

BEGIN;

DROP TABLE IF EXISTS tags;

COMMIT;
