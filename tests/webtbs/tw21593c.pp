program tw21593c;

{$MODE DELPHI}

type
  TWrapper<T> = record
    class procedure Test; static;
  end;

class procedure TWrapper<T>.Test;
var
  size: SizeInt = SizeOf(T);  { Error: Illegal expression }
begin
  Writeln(size);
end;

begin
  TWrapper<Byte>.Test;
end.
