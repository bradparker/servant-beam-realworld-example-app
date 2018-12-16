-- Deploy conduit:create-article-tags to pg
-- requires: create-tags
-- requires: create-articles

BEGIN;

CREATE TABLE IF NOT EXISTS article_tags (
  article__id INT REFERENCES articles(id) ON DELETE CASCADE,
  tag__name TEXT REFERENCES tags(name) ON DELETE CASCADE,
  UNIQUE (article__id, tag__name)
);

COMMIT;
