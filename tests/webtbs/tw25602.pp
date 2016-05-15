{$MODE DELPHI}

type
  TA = class
    const C = 1;
  end;

  TB<T> = object
    procedure Foo;
  end;

procedure TB<T>.Foo;
var
  // X: array[0..T.C] of byte = (0); // Error: Expected another 1 array elements
  X: array[0..T.C] of byte = (0, 2); // Fatal: Syntax error, ")" expected but "," found
begin
  if high(x)<>1 then
    halt(1);
  writeln('ok');
end;

var
  x: TB<TA>;
begin
  x.Foo;
end.
