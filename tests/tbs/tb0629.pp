{ %NORUN }

program tb0629;

{$mode objfpc}

uses
  ub0629, typinfo;

var
  ti: PTypeInfo;
begin
  ti := TTest.specialize GetTypeInfo<LongInt>;
end.
