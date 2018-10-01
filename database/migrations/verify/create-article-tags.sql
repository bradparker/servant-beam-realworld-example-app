-- Verify conduit:create-article-tags on pg

BEGIN;

SELECT * FROM article_tags LIMIT 1;

ROLLBACK;
