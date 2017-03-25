-- By default DDL creates all tables assigned to `postgres` user,
-- but we have specific user `wrapid` and he should be the owner
-- TODO: fix this from Haskell side

-- Tables
FOR tbl IN `psql -qAt -c
  "SELECT tablename FROM pg_tables
   WHERE schemaname = 'public';" wrapid` ;
DO psql -c "ALTER TABLE \"$tbl\" owner TO wrapid" wrapid ;
done

--Sequences
FOR tbl IN `psql -qAt -c
  "SELECT sequence_name FROM information_schema.sequences
   WHERE sequence_schema = 'public';" wrapid` ;
DO psql -c "ALTER TABLE \"$tbl\" owner TO wrapid" wrapid ;
done

--Views
FOR tbl IN `psql -qAt -c
  "SELECT table_name FROM information_schema.views
   WHERE table_schema = 'public';" wrapid` ;
DO psql -c "ALTER TABLE \"$tbl\" owner TO wrapid" wrapid;
done
