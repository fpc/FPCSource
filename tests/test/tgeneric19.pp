{$mode objfpc}
type
  generic tc<T> = class
    procedure p(data : T);
  end;

  tc1 = specialize tc<string>;

procedure tc<T>.p(data : T);
  begin
    readln(data);
    writeln(data);
  end;

begin
end.
