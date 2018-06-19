-- Verify conduit:create-users on pg

BEGIN;

SELECT * FROM users LIMIT 1;

ROLLBACK;
