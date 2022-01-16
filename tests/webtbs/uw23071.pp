unit uw23071;

{$MODE DELPHI}

interface

type
  TWrapper<T> = record
    class procedure Z; static;
  end;

implementation

class procedure TWrapper<T>.Z;
type
  TLocalInteger = Integer;
begin
end;

end.
