program method_init;

{.$mode objfpc}
{$mode delphi}

Type

 { TObj }

 TObj = Class
  class var
   a: record
    b: byte;
   end;
   procedure Test;
 end;

{ TObj }

procedure TObj.Test;
Var

 proc : procedure of object;
 p : pbyte;
begin
  a.b:=5;
  p:=@tobj.create.a.b;
  if p^<>5 then
    halt(1);
end;

procedure UncompilableProc;
Var

 proc : procedure of object;
 p : pbyte;
begin
  tobj.a.b:=6;
  p:=@tobj.create.a.b;
  if p^<>6 then
    halt(2);
end;

begin

  WriteLn('Mode: ', {$IFDEF FPC_DELPHI}'delphi'{$ELSE}'objfpc'{$ENDIF});

  TObj.Create.Test;
  UncompilableProc;

end.
