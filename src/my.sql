CREATE TABLE IF NOT EXISTS pfeg_de.records
    ( id INTEGER UNSIGNED PRIMARY KEY NOT NULL AUTO_INCREMENT
    , surface TEXT
    , lemma TEXT
    ) DEFAULT CHARACTER SET = utf8 DEFAULT COLLATE = utf8_general_ci ENGINE=InnoDB;

CREATE TABLE IF NOT EXISTS pfeg_de.log
    ( id INTEGER UNSIGNED PRIMARY KEY NOT NULL AUTO_INCREMENT
    , action VARCHAR(20)
    , completed DATE
    , version VARCHAR(10)
    , corpusname VARCHAR(20)
    , corpusfile VARCHAR(200)
    ) DEFAULT CHARACTER SET = utf8 DEFAULT COLLATE = utf8_general_ci ENGINE=InnoDB;
