program method_init;

{$mode objfpc}
{.$mode delphi}

Type

 { TObj }

 TObj = Class
   procedure Test;
 end;

{ TObj }

procedure TObj.Test;
Var

 proc : procedure of object;

begin

  proc := {$IFNDEF FPC_DELPHI}@{$ENDIF}TObject.Create.Free;
  WriteLn('Expected TObject actual: ', TObject(TMethod(Proc).Data).ClassName);
  if TObject(TMethod(Proc).Data).ClassName<>'TObject' then
    halt(1);
end;

procedure UncompilableProc;
Var

 proc : procedure of object;

begin

  proc := {$IFNDEF FPC_DELPHI}@{$ENDIF}TObject.Create.Free; // uncompilable in FPC mode
  WriteLn('Expected TObject actual: ', TObject(TMethod(Proc).Data).ClassName);
  if TObject(TMethod(Proc).Data).ClassName<>'TObject' then
    halt(2);
end;

begin

  WriteLn('Mode: ', {$IFDEF FPC_DELPHI}'delphi'{$ELSE}'objfpc'{$ENDIF});

  TObj.Create.Test;
  UncompilableProc;

end.
