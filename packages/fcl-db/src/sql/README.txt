SQL scanner/parser/Abstract Syntax Tree units

This can parse the complete Firebird dialect 3 SQL syntax (which should come pretty close to SQL-92) and builds a syntax tree from it. The Abstract Syntax Tree can re-create the SQL with limited formatting support.

It comes with extensive test suite (over 830 test cases; see the fcl-db\tests directory). It has been tested on almost 400,000 SQL statements. Nevertheless bugs may remain, so any test results you may produce are welcome. Especially the GRANT/REVOKE statements are tested only theoretically.

The scanner/parser have been designed so they should be able to cope with other SQL dialects (using a set of flags) such as MySQL, but this support is currently not implemented.


The purpose is 3-fold:
- Add SQL syntax checking to property editors in the Lazarus IDE.
- Add reverse engineering of database creation scripts to the
  Lazarus Database Desktop (and change the SQL generation to use
  the abstract syntax tree)
- Add the ability to reliably alter queries at runtime.
  (adding fields, filters and whatnot).
