{ %FAIL }

program tw31973;

{$mode objfpc}{$H+}

uses
  Classes, SysUtils
  { you can add units after this },
  gutil, gset;

type
  TIntegerSetCompare = specialize TLess<Integer>;
  TIntegerSet = specialize TSet<Integer, TIntegerSetCompare>;

var
  ISet: TIntegerSet;
  I: Integer;
  Iterator: TSet.TIterator; //should be TIntegerSet.TIterator;
begin
  ISet := TIntegerSet.Create();
  Iterator := ISet.Find(42); //error2014052306.lpr(20,12) Error: Internal error 2014052306
end.
