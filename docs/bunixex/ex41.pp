Program Example41;

{ Program to demonstrate the GetEnv function. }

Uses BaseUnix;

begin
  Writeln ('Path is : ',fpGetenv('PATH'));
end.
