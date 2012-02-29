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
DROP FUNCTION IF EXISTS index_item(TEXT[]);
DROP FUNCTION IF EXISTS query_records(TEXT[], TEXT[][]);
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

-- pos expects array slices a…b in form 'a:b', stuff is a 2-dim array containing text to search for. Example:
-- SELECT query_records('{15:16,9:10}','{{VAFIN,NN},{werden,september}}');
CREATE FUNCTION query_records(pos TEXT[], stuff TEXT[]) RETURNS match_result AS $$
DECLARE
	query1 TEXT := ' ';
	query2 TEXT := ' ';
	andvar TEXT := '';
	rowvar match_result;
	i      INTEGER := 0;
	val    INTEGER[];
	result INTEGER[];
	count  INTEGER := 0;
	temp   TEXT[];
BEGIN
	-- First piece together the queries out of positional parameters and values
	-- Note that this assumes that array_dims(pos) = array_dims(stuff)
	FOREACH temp SLICE 1 IN ARRAY stuff LOOP
		i = i+1;
		val := index_item(temp);
		query1 := query1 || andvar || 'record @> ''' || (val::Text) || '''';
		query2 := query2 || andvar || 'record[' || pos[i] || '] = ''' || (val::Text) || '''';
		andvar = ' AND ';
	END LOOP;
	raise notice 'query1 = %', query1;
	raise notice 'query2 = %', query2;
	RAISE NOTICE 'SELECT counts FROM (SELECT counts FROM records WHERE % ) AS f WHERE %', query1, query2 ;
	FOR val IN EXECUTE 'SELECT counts FROM (SELECT * FROM records WHERE' || query1 || ') AS f WHERE' || query2 LOOP
		count := count+1;
		-- For each of the results of the query, add up the counts arrays positionally.
		FOR i IN SELECT generate_subscripts(val,1) LOOP
			result[i] := COALESCE(result[i],0) + COALESCE(val[i],0);
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
		result[t] := COALESCE(temp,0);
	END LOOP;
	RETURN result;
END;
$$ LANGUAGE plpgsql;
