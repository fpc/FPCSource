unit ugeneric91b;

{$mode objfpc}{$H+}

interface

type
  generic TSomeGeneric2<T> = class
    class procedure Test;
  end;

  TSomeClass2 = class
    class procedure Test;
  end;

implementation

uses
  ugeneric91a;

type
  TSomeGeneric1LongInt = specialize TSomeGeneric1<LongInt>;

class procedure TSomeClass2.Test;
begin
  TSomeGeneric1LongInt.Test;
end;

class procedure TSomeGeneric2.Test;
begin
  Writeln(Self.ClassName);
end;

end.

