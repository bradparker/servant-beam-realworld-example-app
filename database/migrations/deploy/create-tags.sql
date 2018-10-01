-- Deploy conduit:create-tags to pg

BEGIN;

CREATE TABLE IF NOT EXISTS tags (
  name TEXT PRIMARY KEY
);

COMMIT;
