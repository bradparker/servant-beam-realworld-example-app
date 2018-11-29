-- Revert conduit:create-comments from pg

BEGIN;

DROP TABLE IF EXISTS comments;

COMMIT;
