program tanonfunc28;

{$mode objfpc}
{$modeswitch anonymousfunctions}
{$modeswitch functionreferences}

{ test try..except blocks nested in anonymous methods }

uses
  SysUtils;

var
  Acc: Integer;

type
  TProc = reference to procedure;

procedure CallProc(AProc: TProc);
begin
  AProc();
end;

procedure PlainException;
begin
  try
    raise Exception.Create('');
  except on E: Exception do
    Inc(Acc, 4);
  end;
end;

procedure RaisedException;
begin
  try
    CallProc(
      procedure
      begin
        raise Exception.Create('');
      end);
  except on E: Exception do
    Inc(Acc, 30);
  end;
end;

procedure NestedExceptionHandler;
begin
  CallProc(
    procedure
    begin
      try
        raise Exception.Create('');
      except on E: Exception do
        Inc(Acc, 200);
      end;
    end);
end;

procedure TouchInNestedExceptionHandler;
begin
  CallProc(
    procedure
    begin
      try
        raise Exception.Create('');
      except on E: Exception do
        if E.Message = '' then
          Inc(Acc, 1000);
      end;
    end);
end;

begin
  PlainException;
  RaisedException;
  NestedExceptionHandler;
  TouchInNestedExceptionHandler;
  if Acc <> 1234 then
    halt(1);
end.
