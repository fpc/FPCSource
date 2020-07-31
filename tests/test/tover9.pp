unit tover9;

{$mode objfpc}

interface

{ this is the for xmlSchemaSetParserErrors in the xmlschemas.inc of the libxml
  package }

procedure Test(aArg: PLongInt);
function Test(var aArg: LongInt): LongInt;

{ also check generic routines just to be sure }

generic procedure Test2<T>(aArg: PLongInt);
generic function Test2<T>(var aArg: LongInt): LongInt;

implementation

procedure Test(aArg: PLongInt);
begin

end;

function Test(var aArg: LongInt): LongInt;
begin

end;

generic procedure Test2<T>(aArg: PLongInt);
begin

end;

generic function Test2<T>(var aArg: LongInt): LongInt;
begin

end;

end.
