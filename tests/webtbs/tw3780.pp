{ Source provided for Free Pascal Bug Report 3780 }
{ Submitted by "Adriaan van Os" on  2005-03-13 }
{ e-mail: fpc@microbizz.nl }
{$mode gpc}
program func;

type tfun = function( x: real): real;

function f( x: real): real;
begin
        f:= x
end;

function fsum( fun: tfun; x1, x2: real): real;
begin
        fsum:= fun( x1) + fun( x2)
end;

begin
        writeln( fsum( f, 1.0, 2.0))
end.