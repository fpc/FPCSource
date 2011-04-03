{$mode objfpc}

unit gutil;

interface

type generic TLess<T>=class
  class function c(a,b:T):boolean;inline;
end;

type generic TGreater<T>=class
  class function c(a,b:T):boolean;inline;
end;

implementation

class function TLess.c(a,b:T):boolean;inline;
begin
  c:=a<b;
end;

class function TGreater.c(a,b:T):boolean;inline;
begin
  c:=b<a;
end;

end.
