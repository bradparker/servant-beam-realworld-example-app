-- Revert conduit:create-users from pg

BEGIN;

DROP TABLE IF EXISTS users CASCADE;

COMMIT;
