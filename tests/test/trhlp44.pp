{ %NORUN }

program trhlp44;

{$mode delphi}

type
  TTest = record

  end;

  TTestHelper = record helper for TTest
    procedure SayHello(const I: Integer); overload;
    procedure SayHello(const S: string); overload;
  end;

procedure TTestHelper.SayHello(const I: Integer); overload;
begin
  Writeln('Hello ', I);
end;

procedure TTestHelper.SayHello(const S: string); overload;
begin
  Writeln('Hello ', S);
end;

var
  Obj: TTest;
begin
  Obj.SayHello('FPC');
end.
