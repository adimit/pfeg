CREATE TABLE IF NOT EXISTS pfeg_de.records
    ( id INTEGER UNSIGNED PRIMARY KEY NOT NULL AUTO_INCREMENT
    , lcs TEXT
    , rcs TEXT
    , lcl TEXT
    , rcl TEXT
    , target varchar(20) NOT NULL
    ) DEFAULT CHARACTER SET = utf8 DEFAULT COLLATE = utf8_general_ci;

CREATE TABLE IF NOT EXISTS pfeg_de.log
    ( id INTEGER UNSIGNED PRIMARY KEY NOT NULL AUTO_INCREMENT
    , action VARCHAR(10)
    , completed DATE
    , version VARCHAR(10)
    , corpusfile VARCHAR(200)
    , corpusname VARCHAR(10)
    ) DEFAULT CHARACTER SET = utf8 DEFAULT COLLATE = utf8_general_ci;
