{ %FAIL }

{$mode objfpc}
type
 tmyclass = class
  { you should not be able to have virtual methods which 
    are noted as cdecl.
  }
  procedure myroutine;virtual;cdecl;
 end;

procedure tmyclass.myroutine;cdecl;
begin
end;

Begin
end.