{ %VERSION=1.1 }
program testv1;

uses varutils;

Var
  Varia : TVardata;

begin
  Writeln(SizeOf(Varia));
  Writeln('Initializing variant');
  VariantInit(Varia);
  DumpVariant(Varia);
end.
