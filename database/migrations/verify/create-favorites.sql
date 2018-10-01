-- Verify conduit:create-favorites on pg

BEGIN;

SELECT * FROM favorites LIMIT 1;

ROLLBACK;
