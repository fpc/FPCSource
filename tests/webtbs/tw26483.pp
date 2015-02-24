{ %NORUN }

program tw26483;

{$MODE DELPHI}

type
  TA<T> = class
  private
    F: Integer;
  end;

  TB<T> = class
    procedure Foo(A: TObject);
  end;

procedure TB<T>.Foo(A: TObject);
begin
  WriteLn(TA<T>(A).F); // p004.Error: identifier idents no member "F"
end;

begin
end.

