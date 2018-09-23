-- Verify conduit:create-follows on pg

BEGIN;

SELECT * FROM follows LIMIT 1;

ROLLBACK;
