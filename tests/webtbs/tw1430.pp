{ Source provided for Free Pascal Bug Report 1430 }
{ Submitted by "Keith R. Bolson" on  2001-03-07 }
{ e-mail: krbolson@visi.com }
PROGRAM fpc1;


PROCEDURE DoType( b :BOOLEAN; t,f: STRING);
BEGIN
  IF b THEN writeln(t) ELSE writeln(f);
  if b then
    halt(1);
END;

VAR
  ax, ay: Char;

BEGIN
  ax := 'X';  ay := 'Y';
  DoType( ( ([ax, ay] * ['A','C','D']) <> []), 'yes', 'no');
END.
