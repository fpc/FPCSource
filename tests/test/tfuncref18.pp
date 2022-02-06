{ %NORUN }

program tfuncref18;

{$mode objfpc}{$H+}
{$modeswitch functionreferences}

type
  TTest1 = reference to function: TTest1;
  TTest2 = reference to procedure(aArg: TTest2);
  { this needs support for specialize defs to work correctly }
  //TTest3 = reference to function(aArg: specialize TArray<TTest3>): specialize TArray<TTest3>;

begin

end.
