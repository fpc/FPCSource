{ %FAIL }

program tinterface11;

{$mode objfpc}{$H+}
{$interfaces corba}

type
  ITest = interface
    procedure Test;
  end;

  TTest = class(TObject, ITest)
    procedure Test;
  end;

procedure TTest.Test;
begin
end;

var
  t: TTest;
begin
  t := TTest.Create;
  try
    (t as ITest).Test;
  finally
    t.Free;
  end;
end.

