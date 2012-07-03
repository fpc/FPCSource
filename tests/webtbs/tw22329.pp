{ %NORUN }

program tw22329;

{$mode delphi}

type
  TObjectHelper = class helper for TObject
    procedure SayHello(const I: Integer); overload;
    procedure SayHello(const S: string); overload;
  end;

procedure TObjectHelper.SayHello(const I: Integer); overload;
begin
  Writeln('Hello ', I);
end;

procedure TObjectHelper.SayHello(const S: string); overload;
begin
  Writeln('Hello ', S);
end;

var
  Obj: TObject;
begin
  Obj := TObject.Create;
  try
    Obj.SayHello('FPC');
  finally
    Obj.Free;
  end;
end.
