{ $OPT=-Sg }

program bug0035;

{Discovered by Daniel Mantione.}

label   hallo;

begin
   writeln('Hello');
  begin
hallo:          {Error message: Incorrect expression.}
  end;
  writeln('Hello again');
end.
