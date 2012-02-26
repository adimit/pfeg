CREATE TABLE IF NOT EXISTS unigrams
    ( id SERIAL PRIMARY KEY
    , form TEXT UNIQUE NOT NULL
    , count INTEGER NOT NULL);

CREATE TABLE IF NOT EXISTS records
    ( id sha256 PRIMARY KEY
    , record INTEGER[] NOT NULL
    , counts INTEGER[] NOT NULL );

-- Clean up first.
DROP FUNCTION IF EXISTS records_upsert(TEXT,INTEGER,TEXT[]);
DROP FUNCTION IF EXISTS unigram_upsert(TEXT,INTEGER);
DROP FUNCTION IF EXISTS match_function(INTEGER[],TEXT[]);
DROP FUNCTION IF EXISTS index_item(TEXT[]);
DROP TYPE IF EXISTS match_result;

CREATE FUNCTION unigram_upsert (f TEXT, c INTEGER) RETURNS VOID AS $$
BEGIN
	LOOP
		UPDATE unigrams SET count = count+c WHERE form=f;
		IF found THEN
			RETURN;
		END IF;
		BEGIN
			INSERT INTO unigrams(form,count) VALUES (f,c);
			RETURN;
		EXCEPTION WHEN unique_violation THEN
			NULL; -- attempt to update again
		END;
	END LOOP;
END;
$$ LANGUAGE plpgsql;

CREATE TYPE match_result AS (
	aggres	integer[],
	matches	integer
);

CREATE FUNCTION match_function(pos integer[], v TEXT[]) RETURNS match_result AS $$
DECLARE
	temp   INTEGER[];
	result INTEGER[];
	val    INTEGER[] := index_item(v);
	i      INTEGER := 0;
	count  INTEGER := 0;
	query  TEXT    := ' ';
	andvar TEXT    := '';
	rowvar match_result;
BEGIN
	-- First piece together the query out of positional parameters and values
	-- Note that this assumes that array_dims(pos) = array_dims(val)
	FOR i IN SELECT generate_subscripts(pos,1) LOOP
		query := query || andvar || 'record[' || pos[i] || ']=' || val[i];
		andvar := ' AND ';
	END LOOP;
	FOR temp IN EXECUTE 'SELECT counts FROM records WHERE' || query LOOP
		count := count+1;
		-- For each of the results of the query, add up the counts arrays positionally.
		RAISE NOTICE 'temp array has values: %' , temp;
		FOR i IN SELECT generate_subscripts(temp,1) LOOP
			result[i] := result[i] + temp[i];
		END LOOP;
	END LOOP;
	rowvar := ROW(result,count);
	RETURN rowvar;
END;
$$ LANGUAGE plpgsql;

CREATE FUNCTION records_upsert(h text, t INTEGER, r TEXT[]) RETURNS VOID AS $$
DECLARE
	record_hash sha256    := sha256(h);
	indexed_r   INTEGER[] := index_item(r);
BEGIN
	LOOP
		UPDATE records SET counts[t] = counts[t]+1 WHERE id=record_hash;
		IF found THEN
			RETURN;
		END IF;
		DECLARE
			temp_arr int[]; -- we insert this array to initialize counts
		BEGIN
			temp_arr[t] := 1;
			INSERT INTO records(id,record,counts) VALUES (record_hash,indexed_r,temp_arr);
			RETURN;
		EXCEPTION WHEN unique_violation THEN
			NULL; -- attempt to update again
		END;
	END LOOP;
END;
$$ LANGUAGE plpgsql;

CREATE FUNCTION index_item(item TEXT[]) RETURNS INTEGER[] AS $$
DECLARE
	result INTEGER[];
	t      INTEGER := 0;
	temp   INTEGER := 0;
BEGIN
	FOR t IN SELECT generate_subscripts(item,1) LOOP
		SELECT id INTO temp FROM unigrams WHERE form = item[t];
		result[t] := temp;
	END LOOP;
	RETURN result;
END;
$$ LANGUAGE plpgsql;
