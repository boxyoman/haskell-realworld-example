
CREATE TABLE IF NOT EXISTS "user"
  ( user_id bigserial NOT NULL
  , email Text NOT NULL
  , username Text NOT NULL
  , bio Text NOT NULL
  , image Text NOT NULL
  , password Text NOT NULL
  , created_at timestamptz NOT NULL DEFAULT NOW()
  , updated_at timestamptz NOT NULL DEFAULT NOW()
  , PRIMARY KEY (user_id)
  , UNIQUE (email)
  , UNIQUE (username)
  );

CREATE TABLE IF NOT EXISTS following
  ( user_id bigint NOT NULL
  , following bigint NOT NULL
  , PRIMARY KEY (user_id, following)
  , FOREIGN KEY (user_id) REFERENCES "user" (user_id)
  , FOREIGN KEY (following) REFERENCES "user" (user_id)
  );

CREATE TABLE IF NOT EXISTS article
  ( article_id bigserial NOT NULL
  , user_id bigint NOT NULL
  , slug Text NOT NULL
  , title Text NOT NULL
  , description Text NOT NULL
  , body Text NOT NULL
  , created_at timestamptz NOT NULL DEFAULT NOW()
  , updated_at timestamptz NOT NULL DEFAULT NOW()
  , PRIMARY KEY (article_id)
  , FOREIGN KEY (user_id) REFERENCES "user" (user_id)
  );

CREATE TABLE IF NOT EXISTS article_tag
  ( article_id bigint NOT NULL
  , tag Text NOT NULL
  , PRIMARY KEY (article_id, tag)
  , FOREIGN KEY (article_id) REFERENCES article (article_id) ON DELETE CASCADE
  );

CREATE TABLE IF NOT EXISTS favorites
  ( article_id bigint NOT NULL
  , user_id bigint NOT NULL
  , PRIMARY KEY (article_id, user_id)
  , FOREIGN KEY (article_id) REFERENCES article (article_id) ON DELETE CASCADE
  , FOREIGN KEY (user_id) REFERENCES "user" (user_id)
  );

CREATE TABLE IF NOT EXISTS comments
  ( comment_id bigserial NOT NULL
  , user_id bigint NOT NULL
  , article_id bigint NOT NULL
  , body Text NOT NULL
  , created_at timestamptz NOT NULL DEFAULT NOW()
  , updated_at timestamptz NOT NULL DEFAULT NOW()
  , PRIMARY KEY (comment_id)
  , FOREIGN KEY (article_id) REFERENCES article (article_id) ON DELETE CASCADE
  , FOREIGN KEY (user_id) REFERENCES "user" (user_id)
  );
