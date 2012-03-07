CREATE TABLE IF NOT EXISTS pfeg_de.records
    ( id INTEGER UNSIGNED PRIMARY KEY NOT NULL AUTO_INCREMENT
    , lcs TEXT
    , rcs TEXT
    , lcl TEXT
    , rcl TEXT
    , target varchar(20) NOT NULL
    );

CREATE TABLE IF NOT EXISTS pfeg_de.log
    ( id INTEGER UNSIGNED PRIMARY KEY NOT NULL AUTO_INCREMENT
    , action VARCHAR(10)
    , completed DATE
    , version VARCHAR(10)
    , corpusfile VARCHAR(200)
    , corpusname VARCHAR(10) );
