Program Example46;

{ Program to demonstrate the FSearch function. }

Uses BaseUnix, Unix, Strings;

begin
  Writeln ('ls is in : ',FSearch ('ls',strpas(fpGetenv('PATH'))));
end.
