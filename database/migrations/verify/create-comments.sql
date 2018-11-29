-- Verify conduit:create-comments on pg

BEGIN;

SELECT * FROM comments LIMIT 1;

ROLLBACK;
