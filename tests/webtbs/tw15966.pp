{ %norun }
{ %opt=-vh -Seh }
program tw15966;

uses
  uw15966;

procedure test(c: char); public; overload;
begin
  writeln(c);
end;

begin
  test(0);
end.
