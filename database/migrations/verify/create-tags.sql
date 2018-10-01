-- Verify conduit:create-tags on pg

BEGIN;

SELECT * FROM tags LIMIT 1;

ROLLBACK;
