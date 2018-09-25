-- Deploy conduit:create-articles to pg
-- requires: create-users

BEGIN;

CREATE TABLE IF NOT EXISTS articles (
  id SERIAL PRIMARY KEY,
  body TEXT NOT NULL,
  slug TEXT UNIQUE NOT NULL,
  title TEXT NOT NULL,
  description TEXT NOT NULL,
  author__id INT REFERENCES users(id),
  created_at TIMESTAMP WITH TIME ZONE NOT NULL,
  updated_at TIMESTAMP WITH TIME ZONE NOT NULL
);

COMMIT;
