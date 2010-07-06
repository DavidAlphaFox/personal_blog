DELETE FROM sessions
WHERE ((expire IS NULL)     AND datetime(mtime, 'unixepoch') < datetime('now', '-3 days'))
        OR
      ((expire IS NOT NULL) AND datetime(expire, 'unixepoch') < datetime('now'));

VACUUM;
