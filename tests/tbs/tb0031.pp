{ Old file: tbs0035.pp }
{  label at end of block gives error                   OK 0.9.9 (FK) }

{$goto on}

label   hallo;

begin
   writeln('Hello');
  begin
hallo:          {Error message: Incorrect expression.}
  end;
  writeln('Hello again');
end.
