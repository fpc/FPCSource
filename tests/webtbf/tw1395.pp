{ %FAIL }
{ Source provided for Free Pascal Bug Report 1395 }
{ Submitted by "Stian Skjelstad" on  2001-02-08 }
{ e-mail: stian@lilo.no }
var
 a, b : pointer;

begin
 a^:=b^;
end.
