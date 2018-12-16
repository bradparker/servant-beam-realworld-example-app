-- Deploy conduit:create-follows to pg
-- requires: create-users

BEGIN;

CREATE TABLE IF NOT EXISTS follows (
  follower__id INT REFERENCES users(id) ON DELETE CASCADE,
  followee__id INT REFERENCES users(id) ON DELETE CASCADE
);

COMMIT;
