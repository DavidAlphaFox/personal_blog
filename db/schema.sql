CREATE TABLE users
(
    email       TEXT        PRIMARY KEY NOT NULL,   -- email, used for login
    passwd      VARCHAR(40)             NOT NULL,   -- SHA1 of the password
    first_name  TEXT                    NULL,
    last_name   TEXT                    NULL,
    nick_name   TEXT                    NULL,
    displ_name  TEXT                    NULL,
    visual_edit INTEGER                 DEFAULT 1,  -- 1 if the user wants a visual editor
    role        TEXT                    NOT NULL,   -- permissions
    website     TEXT                    NULL        -- user website URL
);

CREATE TABLE posts
(
    post_id     INTEGER     PRIMARY KEY NOT NULL,
    author      TEXT                    NOT NULL,                   -- FK to users.email
    ctime       VARCHAR(32)             DEFAULT (datetime('now')),  -- creation timestamp. Format '2009-02-09 21:44:05', GMT
    mtime       VARCHAR(32)             DEFAULT (datetime('now')),  -- last mod. timestamp. Format '2009-02-09 21:44:05', GMT
    post_type   VARCHAR(16)             NOT NULL,                   -- "post" or "page"
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

CREATE TABLE posts_tree -- makes sense only for "page" posts
(
    page_id     INTEGER     PRIMARY KEY NOT NULL,   -- FK to posts.post_id
    parent_page INTEGER                 NULL,       -- FK to posts.post_id (null -> root page)
    position    INTEGER                 DEFAULT 0   -- position of this page in the children list
);

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
    approved        INTEGER                 DEFAULT 0                   -- 0 -> not approved; 1 -> comment approved
);

CREATE TABLE tags
(
    tag_id      INTEGER     PRIMARY KEY NOT NULL,
    tag         TEXT                    NOT NULL
);

CREATE TABLE tags_tree  -- parent-children relationship over tags
(
    tag_id      INTEGER     PRIMARY KEY NOT NULL,   -- FK to tags.tag_id
    tag_parent  INTEGER                 NULL        -- FK to tags.tag_id (null -> root tag)
);

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

