CREATE TABLE sessions   -- Sessions
(
    key                 TEXT    PRIMARY KEY NOT NULL,
    data                TEXT                NOT NULL,
    ctime               REAL                NOT NULL,
    mtime               REAL                NOT NULL,
    expire              REAL                NULL,
    client_ip           TEXT                NULL,
    client_user_agent   TEXT                NULL
);

CREATE INDEX ctime_idx ON sessions (ctime ASC);
CREATE INDEX mtime_idx ON sessions (mtime ASC);
CREATE INDEX expire_idx ON sessions (expire ASC);

