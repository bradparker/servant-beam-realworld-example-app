-- Deploy conduit:create-favorites to pg
-- requires: create-articles
-- requires: create-users

BEGIN;

CREATE TABLE IF NOT EXISTS favorites (
  user__id INT REFERENCES users(id),
  article__id INT REFERENCES articles(id)
);

COMMIT;
