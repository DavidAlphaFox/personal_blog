---------------------
-- DATABASE SCHEMA --
---------------------

CREATE TABLE users
(
    email       TEXT        PRIMARY KEY NOT NULL,   -- email, used for login
    passwd      VARCHAR(40)             NOT NULL,   -- SHA1 of the password
    first_name  TEXT                    NULL,
    last_name   TEXT                    NULL,
    displ_name  TEXT                    NULL,
    visual_edit INTEGER                 DEFAULT 1,  -- 1 if the user wants a visual editor
    role        TEXT                    NOT NULL,   -- permissions, one of:
                                                    --      "admin", "editor", "author",
                                                    --      "contributor", "subscriber"
    website     TEXT                    NULL        -- user website URL
);

CREATE TABLE posts
(
    post_id     INTEGER     PRIMARY KEY NOT NULL,
    author      TEXT                    NOT NULL,                   -- FK to users.email
    ctime       VARCHAR(32)             DEFAULT (datetime('now')),  -- creation timestamp. Format '2009-02-09 21:44:05', GMT
    mtime       VARCHAR(32)             DEFAULT (datetime('now')),  -- last mod. timestamp. Format '2009-02-09 21:44:05', GMT
    status      INTEGER                 DEFAULT 0,                  -- 1 if the post is published, 0 for a draft
    title       TEXT                    NULL,                       -- post title
    url_title   TEXT                    NULL,                       -- URL aware version of the title
    content     TEXT                    NULL,                       -- post content
    summary     TEXT                    NULL,                       -- post summary
    comments    INTEGER                 DEFAULT 1,                  -- 1 if comments are allowed
    pings       INTEGER                 DEFAULT 1,                  -- 1 if pings are allowed
    private     INTEGER                 DEFAULT 0,                  -- 1 if the post is private
    pinged      TEXT                    NULL                        -- a list of '\n' separeted URLs that have been pinged
);

CREATE TABLE pages
(
    page_id     INTEGER     PRIMARY KEY NOT NULL,
    mtime       VARCHAR(32)             DEFAULT (datetime('now')),  -- last mod. timestamp. Format '2009-02-09 21:44:05', GMT
    title       TEXT                    NULL,                       -- post title
    url_title   TEXT                    NOT NULL,                   -- URL aware version of the title
    template    TEXT                    NOT NULL,                   -- pathname of the template
    password    TEXT                    NULL                        -- Optiona password to protect the page
);

CREATE TABLE pages_tree
(
    page_id     INTEGER     PRIMARY KEY NOT NULL,   -- FK to pages.page_id
    parent_page INTEGER                 NULL,       -- FK to pages.page_id (null -> root page)
    position    INTEGER                 DEFAULT 0,  -- position of this page in the children list
    CONSTRAINT pages_tree_idx UNIQUE (parent_page, position)
);

CREATE VIEW pages_complete AS
  SELECT * FROM pages JOIN pages_tree USING(page_id)
           ORDER BY page_id;

CREATE TABLE comments
(
    comment_id      INTEGER     PRIMARY KEY NOT NULL,
    post_id         INTEGER                 NOT NULL,                   -- FK to posts.post_id
    author_name     TEXT                    NOT NULL,                   -- author display name
    author_email    TEXT                    NOT NULL,                   -- author email address
    author_url      TEXT                    NULL,                       -- author URL
    author_ip       TEXT                    NOT NULL,                   -- IP address of the author's client
    author_ua       TEXT                    NOT NULL,                   -- user agent of the author's client
    comment_time    VARCHAR(32)             DEFAULT (datetime('now')),  -- timestamp. Format '2009-02-09 21:44:05', GMT
    content         TEXT                    NOT NULL,                   -- comment content
    approved        INTEGER                 DEFAULT 0,                  -- 0 -> not approved; 1 -> comment approved
    spam            INTEGER                 DEFAULT 0                   -- 0 -> not spam; 1 -> marked as spam
);

CREATE TABLE tags
(
    tag_id      INTEGER     PRIMARY KEY NOT NULL,
    tag_text    TEXT                    NOT NULL,
    tag_slug    TEXT                    NOT NULL
);

CREATE TABLE tags_tree  -- parent-children relationship over tags
(
    tag_id      INTEGER     PRIMARY KEY NOT NULL,   -- FK to tags.tag_id
    tag_parent  INTEGER                 NULL        -- FK to tags.tag_id (null -> root tag)
);

CREATE VIEW tags_adj AS
  SELECT * FROM tags JOIN tags_tree USING(tag_id);

CREATE TABLE posts_tags_rel
(
    post_id     INTEGER                 NOT NULL,   -- FK to posts.post_id
    tag_id      INTEGER                 NOT NULL,   -- FK to tags.tag_id
    CONSTRAINT posts_tags_rel_pkey PRIMARY KEY (post_id, tag_id)
);

CREATE TABLE links
(
    link_id     INTEGER     PRIMARY KEY NOT NULL,   -- id of the link
    url         TEXT                    NOT NULL,   -- URL
    name        TEXT                    NOT NULL,   -- name to be displayed
    description TEXT                    NULL,       -- description -> title of the <a> element
    visible     INTEGER                 DEFAULT 1,  -- 0 -> link is not displayed
    link_rel    TEXT                    NULL        -- space separated list of these tokens:
                                                    --     "identity" "contact" "acquaintance"
                                                    --     "friend" "met" "co-worker" "colleague"
                                                    --     "co-resident" "neighbor" "child" "kin"
                                                    --     "parent" "sibling" "spouse" "muse"
                                                    --     "crush" "date" "sweetheart"
);

CREATE TABLE options
(
    plugin      TEXT                    NOT NULL,   -- plugin name, "__BLOB__" for global options
    name        TEXT                    NOT NULL,   -- option name
    value       TEXT                    NOT NULL,   -- option value
    CONSTRAINT options_pkey PRIMARY KEY (plugin, name)
);

CREATE TABLE tokens     -- One shot tokens
(
    value       VARCHAR(32)     PRIMARY KEY   NOT NULL,                 -- tipically a MD5 digest of something relevant
    ctime       VARCHAR(32)                   DEFAULT (datetime('now')) -- creation timestamp. Format '2009-02-09 21:44:05', GMT
);

CREATE TABLE short_urls   -- Support for short URLs service
(
    id          INTEGER         PRIMARY KEY   NOT NULL,                   -- ID for the URL
    id_enc      VARCHAR(16)                   NOT NULL,                   -- ID, encoded with Base62
    long_url    TEXT                          NOT NULL,                   -- the original long URL
    atime       VARCHAR(32)                   DEFAULT (datetime('now'))   -- last access timestamp.
                                                                          --   Format '2009-02-09 21:44:05', GMT
);
CREATE UNIQUE INDEX short_urls__id_enc__idx   ON short_urls (id_enc);
CREATE UNIQUE INDEX short_urls__long_url__idx ON short_urls (long_url);
CREATE        INDEX short_urls__atime__idx    ON short_urls (atime);


--------------------
-- DEFAULT VALUES --
--------------------
-- My user -- passwd = 'foo' --
INSERT INTO users (email, passwd, first_name, last_name,
                   displ_name, visual_edit, role, website)
       VALUES ('p.donadeo@foobaz.com',
               '0beec7b5ea3f0fdbc95d0dd47f3c5bc275da8a33', 'Paolo', 'Donadeo',
               'Paolo Donadeo', 1, 'admin', 'http://www.donadeo.net/');

-- Options --
INSERT INTO options (plugin, name, value) VALUES
    ('__BLOG__', 'blog_title', 'Paolo Donadeo &#8212; LifeLOG');
INSERT INTO options (plugin, name, value) VALUES
    ('__BLOG__', 'tagline', 'All about my life, job and thoughts');
INSERT INTO options (plugin, name, value) VALUES
    ('__BLOG__', 'my_name', 'Paolo Donadeo');
INSERT INTO options (plugin, name, value) VALUES
    ('__BLOG__', 'my_email_address', 'p.donadeo@foobaz.com');
INSERT INTO options (plugin, name, value) VALUES
    ('__BLOG__', 'akismet_key', 'ae48bd20be8f');
INSERT INTO options (plugin, name, value) VALUES
    ('__BLOG__', 'admin_post_per_page', '15');
INSERT INTO options (plugin, name, value) VALUES
    ('__BLOG__', 'smtp_pwd', 'foo');
INSERT INTO options (plugin, name, value) VALUES
    ('__BLOG__', 'blog_post_per_page', '7');
INSERT INTO options (plugin, name, value) VALUES
    ('__BLOG__', 'blog_post_in_rss', '10');
INSERT INTO options (plugin, name, value) VALUES
    ('__BLOG__','blog_hostname','www.donadeo.net');

