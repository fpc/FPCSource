{ %fail }
{$mode objfpc}
type
  generic tc<T> = class
    procedure p(data : T);
  end;

  tr = record
  end;

  tc1 = specialize tc<tr>;

procedure tc.p(data : T);
  begin
    readln(data);
    writeln(data);
  end;

begin
end.
