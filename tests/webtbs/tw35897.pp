{ %opt=-Sew }

{$mode iso}
program test(input, output);

procedure doit(n : integer);
begin
  writeln(n mod 10);
end;

begin
   doit(23);
end. 
