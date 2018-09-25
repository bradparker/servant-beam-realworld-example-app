-- Revert conduit:create-articles from pg

BEGIN;

DROP TABLE IF EXISTS articles CASCADE;

COMMIT;
