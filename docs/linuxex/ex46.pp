Program Example46;

{ Program to demonstrate the FSearch function. }

Uses linux,strings;

begin
  Writeln ('ls is in : ',FSearch ('ls',strpas(Getenv('PATH'))));
end.
