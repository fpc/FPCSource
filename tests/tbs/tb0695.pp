program tb0695;

{$APPTYPE CONSOLE}
{$IFDEF FPC}
  {$MODE OBJFPC}
  {$MODESWITCH ADVANCEDRECORDS}
{$ENDIF}

type
  TState = record
  strict private
    FState: LongWord;
    class operator Initialize({$IFDEF FPC}var{$ELSE}out{$ENDIF} Instance: TState);
  public
    property State: LongWord read FState;
  end;

var
  initdone: Boolean = False;

class operator TState.Initialize({$IFDEF FPC}var{$ELSE}out{$ENDIF} Instance: TState);
begin
  Writeln('TState now being initialised');
  initdone := True;
  Instance.FState := 1;
end;

type
  TSys0 = class(TObject)
    FItem: TState;
    constructor Create;
  end;

  TSys = class(TSys0)
    //FItem2: TState;
  end;

constructor TSys0.Create;
begin
  inherited;
  Writeln(ClassName + ' now being created');
  Writeln('State=', FItem.State);
end;

var
  C: TSys;
begin
  Writeln('Create instance of TSys...');
  C := TSys.Create;
  if not InitDone then
    Halt(1);
  C.Free;
end.
