program tw21593b;

{$MODE DELPHI}

type
  TWrapper<T> = record
  strict private
    const Size = SizeOf(T);  { Error: Illegal expression }
  public
    class procedure Test; static;
  end;

class procedure TWrapper<T>.Test;
begin
  Writeln(Size);
end;

begin
  TWrapper<Byte>.Test;
end.
