{ Old file: tbf0036.pp }
{  assigning a single character to array of char      ?OK 0.9.9 }

program bug0036;

{Discovered by Daniel Mantione.}

var     a:array[0..31] of char;

begin
   a:=' ';      {Incorrect Pascal statement, but why a protection error?}
end.
