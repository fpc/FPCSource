{ %FAIL }
{ %OPT=-Sew }

{ Old file: tbf0351.pp }

{$MACRO OFF}

{ The next line should give a Warning that macro support not has
  been turned on }
{$define mac1 := writeln('test')}

begin
end.
