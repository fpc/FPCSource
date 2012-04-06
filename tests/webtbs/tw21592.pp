{$MODE DELPHI}

type
  TBytesOverlay<T> = array [0..SizeOf(T) - 1] of Byte;
    { Error: Identifier not found "T" }

var
  a : TBytesOverlay<Byte>;

begin
  if sizeof(a)<>1 then
    halt(1);
  writeln('ok');
end.
