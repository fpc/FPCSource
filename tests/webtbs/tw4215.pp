{ Source provided for Free Pascal Bug Report 4215 }
{ Submitted by "Tony Maro" on  2005-07-24 }
{ e-mail: tony@maro.net }
uses
  sysutils;
var
   MyCurrency: Currency;
begin
   CurrencyFormat := 0; // optional? It's my default anyway.
   MyCurrency := 12.53;
   writeln(MyCurrency); // this works
   writeln(format('%n',[MyCurrency])); // this doesn't
end.
