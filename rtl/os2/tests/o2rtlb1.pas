program testread;
{uses crt;}
var
  cadena,cadena2 : string;
  number : real;
begin
  {clrscr;}
  cadena2 := 'Previous string';
  write ('Enter the string ');
  readln (cadena);
  writeln ('You entered ',cadena);
  writeln ('Previous string was ',cadena2);
  write ('Enter a number ');
  readln (number);
  writeln ('Number entered was ',number);
  readln;
end.
