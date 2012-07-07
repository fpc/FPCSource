program tw21593a;

{$MODE DELPHI}

type
  TWrapper<T> = record
    class procedure Test; static;
  end;

class procedure TWrapper<T>.Test;
const
  Size = SizeOf(T);  { Error: Illegal expression }
begin
  Writeln(Size);
end;

begin
  TWrapper<Byte>.Test;
end.
