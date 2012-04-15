{ %NORUN }
{ %RECOMPILE }

program tgeneric74;

{$mode objfpc}

uses
  ugeneric74a;

type
  TSpezLongInt = specialize TGeneric<LongInt>;
begin

end.
