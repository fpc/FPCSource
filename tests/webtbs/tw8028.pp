program test;

{$mode objfpc}
{$inline on}

type
  TTest = procedure of object;

  TMyRecord = record
    Test: TTest;
  end;

  TMyObject = class
    procedure Test;
  end;

function TMyRecordMake(const Test: TTest): TMyRecord; inline;
begin
  Result.Test := Test;
end;

procedure TMyObject.Test;
begin
  TMyRecordMake(nil);
end;

begin
end.
