-- Revert conduit:create-favorites from pg

BEGIN;

DROP TABLE IF EXISTS favorites;

COMMIT;
