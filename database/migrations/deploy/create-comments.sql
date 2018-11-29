-- Deploy conduit:create-comments to pg
-- requires: create-articles
-- requires: create-users

BEGIN;

CREATE TABLE IF NOT EXISTS comments (
  id SERIAL PRIMARY KEY,
  body TEXT NOT NULL,
  author__id INT REFERENCES users(id),
  article__id INT REFERENCES articles(id),
  created_at TIMESTAMP WITH TIME ZONE NOT NULL,
  updated_at TIMESTAMP WITH TIME ZONE NOT NULL
);

COMMIT;
