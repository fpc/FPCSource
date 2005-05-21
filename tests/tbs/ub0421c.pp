unit ub0421c;
interface

{$mode objfpc}

type
  cl1=class
    f1:longint;
    constructor create;
  end;

implementation

    constructor cl1.create;
    begin
      f1 := 10;
    end;

end.
