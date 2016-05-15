unit uw26922b;

{$mode objfpc}{$H+}

interface

Type
  generic TTestObjectAbstract<T> = class
    private
      var
        FTest : T;
  end;

implementation

uses
  uw26922a;

end.

