program project1;

{$mode objfpc}{$H+}

type
  TConstHolder = class
  public
    const
      C = 10;
  end;

  TSimple = class
    Arr: array [0..TConstHolder.C] of Integer; //this works
  end;


  generic TGeneric <T> = class
    Arr: array [0..T.C] of Integer; //but here is error
// Can't evaluate constant expression

  end;

begin
end.
