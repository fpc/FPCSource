{$MODE DELPHI}

type
  TA = class
    const C = 1;
  end;

  TB<T> = class
    procedure Foo;
  end;

procedure TB<T>.Foo;
var
  i: Integer = 0;
begin
  // i := i + T.C; // <- is ok
  Inc(i, T.C); // Error: Incompatible types: got "untyped" expected "LongInt"
  if i<>1 then
    halt(1);
end;
var
  B : TB<TA>;
begin
  B:=TB<TA>.Create;
  B.Foo;
  B.Free;
  writeln('ok');
end.
