unit tb0724;

{$mode delphi}{$H+}

interface

type
  ITest<T> = interface
  end;

implementation

uses
  { when the compiler reaches ucycleb then depending on the order either ub0724b
    or ub0724c will have neither its global- nor localsymtable set when the
    compiler specializes ITest<>, because the compiler didn't yet reach the
    point to compile either ucyclec or ucycled }
  ub0724b,
  ub0724a,
  ub0724c;

end.

