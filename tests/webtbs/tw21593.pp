{$MODE DELPHI}

type
  TWrapper<T> = record
    strict private
      const Size1 = SizeOf(T);  { Error: Illegal expression }
    class procedure Test; static;
  end;

class procedure TWrapper<T>.Test;
const
  Size2 = SizeOf(T);  { Error: Illegal expression }
var
  size3: SizeInt = SizeOf(T);  { Error: Illegal expression }
begin
  Writeln(Size3);
end;

var
  Wrapper : TWrapper<Byte>;

begin
end.
