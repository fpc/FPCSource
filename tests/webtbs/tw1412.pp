PROGRAM initializationBug( INPUT, OUTPUT );
{$H+}
CONST
  bufferSize = 8;
  tableSize = 2;

TYPE
  bufferRecord = RECORD
    stringBuffer     : String;
    characterBuffer  : ARRAY[ 1..bufferSize ] OF CHAR;
  END;

VAR
  bufferTable  : ARRAY[ 1..tableSize ] OF bufferRecord;

BEGIN
  WRITELN( '< INITIALIZATION BUG HAS NOT OCCURRED!' );
END.
