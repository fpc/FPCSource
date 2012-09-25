unit ugeneric91a;

{$mode objfpc}{$H+}

interface

type
  generic TSomeGeneric1<T> = class
    class procedure Test;
  end;

  TSomeClass1 = class
    class procedure Test;
  end;

implementation

uses
  ugeneric91b;

type
  TSomeGeneric2LongInt = specialize TSomeGeneric2<LongInt>;

class procedure TSomeClass1.Test;
begin
  TSomeGeneric2LongInt.Test;
end;

class procedure TSomeGeneric1.Test;
begin
  Writeln(Self.ClassName);
end;

end.

