CREATE TABLE users
  ( id SERIAL PRIMARY KEY NOT NULL UNIQUE
  , username TEXT NOT NULL UNIQUE
  , email TEXT NOT NULL UNIQUE
  , password CHAR(60) NOT NULL -- bcrypt hash and salt
  , created_at TIMESTAMP WITH TIME ZONE NOT NULL
  , updated_at TIMESTAMP WITH TIME ZONE NOT NULL
  );

CREATE TABLE music
  ( id SERIAL PRIMARY KEY NOT NULL UNIQUE
  , user_id INT NOT NULL
  , composer_first_name TEXT NOT NULL
  , composer_middle_name TEXT NULL
  , composer_last_name TEXT NOT NULL
  , title TEXT NOT NULL
  , movement_name TEXT NULL
  , movement_number INT NULL
  , start_measure INT NOT NULL
  , end_measure INT NOT NULL
  , created_at TIMESTAMP WITH TIME ZONE NOT NULL
  , updated_at TIMESTAMP WITH TIME ZONE NOT NULL
  , contents xml NOT NULL
  , FOREIGN KEY (user_id) REFERENCES users (id)
  );

CREATE TABLE fingering
  ( id SERIAL PRIMARY KEY NOT NULL UNIQUE
  , user_id INT NOT NULL
  , music_id INT NOT NULL
  , description TEXT NULL
  , created_at TIMESTAMP WITH TIME ZONE NOT NULL
  , updated_at TIMESTAMP WITH TIME ZONE NOT NULL
  , contents xml NOT NULL
  , FOREIGN KEY (user_id) REFERENCES users (id)
  , FOREIGN KEY (music_id) REFERENCES music (id)
  );

CREATE TABLE comments
  ( id SERIAL PRIMARY KEY NOT NULL UNIQUE
  , user_id INT NOT NULL
  , fingering_id INT NOT NULL
  , created_at TIMESTAMP WITH TIME ZONE NOT NULL
  , updated_at TIMESTAMP WITH TIME ZONE NOT NULL
  , comment_text json NOT NULL
  , FOREIGN KEY (user_id) REFERENCES users (id)
  , FOREIGN KEY (fingering_id) REFERENCES fingering (id)
  );
