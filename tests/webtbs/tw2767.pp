{ Source provided for Free Pascal Bug Report 2767 }
{ Submitted by "Jakub Cernohorsky" on  2003-11-06 }
{ e-mail: cernoj@fit.vutbr.cz }
var a:integer;
    err:boolean;
begin
  a:=25;
  case a of
    -10..0:writeln('-10..0');
    31:writeln('31');
    32..38:begin
             writeln('32-38');
             err:=true;
           end;
    66:writeln('66');
  end;
  if err then
    halt(1);
end.
