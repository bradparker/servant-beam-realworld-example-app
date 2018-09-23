-- Revert conduit:create-follows from pg

BEGIN;

DROP TABLE IF EXISTS follows;

COMMIT;
