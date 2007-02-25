{ %fail }
unit tw8398;
{ found error by s0t // SotSoft 07-02-24 }
interface

type
  a = record
    c: char;
  end;

implementation

procedure x;
begin
  with a do
  with b do
end;


begin
end.
