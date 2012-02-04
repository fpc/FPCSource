{ %NORUN }
{ %RECOMPILE }

program tgeneric75;

{$mode objfpc}

uses
  ugeneric75;

type
  TSpezLongInt = specialize TGeneric<LongInt>;

begin

end.
