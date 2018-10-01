-- Revert conduit:create-article-tags from pg

BEGIN;

DROP TABLE IF EXISTS article_tags;

COMMIT;
