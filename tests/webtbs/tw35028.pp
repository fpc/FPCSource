{ %OPT=-gh }

program tw35028;

{$mode objfpc}

uses
{$ifdef unix}
  cthreads,
{$endif}
  Classes;

type
  TTest = class
    procedure Test;
  end;

procedure TTest.Test;
begin
  raise TObject.Create;
end;

var
  t: TTest;
begin
  HaltOnNotReleased := True;
  try
    TThread.Queue(Nil, @t.Test);
  except
  end;
end.
