
{ Source provided for Free Pascal Bug Report 2432 }
{ Submitted by "Alfred Gaschler" on  2003-03-21 }
{ e-mail:  Alfred.Gaschler@t-online.de }

program test;

uses
  classes;

const
  has_errors : boolean = false;

procedure a(p : array of tpoint);
var
  n : integer;
begin
  for n := low(p) to high(p) do
    begin
      write(p[n].x,' ',p[n].y,' ');
      if p[n].x<>n+1 then
        begin
          Writeln('Error');
          has_errors:=true;
        end;
      //the result is 1 1 2 2 5 5 4 4 5 5
      //should be 1 1 2 2 3 3 4 4 5 5
    end;
  writeln;
end;

begin
  a([point(1,1),point(2,2),point(3,3),
     point(4,4),point(5,5)]);
  if has_errors then
    Halt(1);
end.
