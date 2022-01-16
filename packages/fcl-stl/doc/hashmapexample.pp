{$mode objfpc}

{define oldstyleiterator}

uses ghashmap;

type hashlli=class
       public
       class function hash(a:longint; b:SizeUInt):SizeUInt;
     end;
     maplli=specialize THashMap<longint, longint, hashlli>;

class function hashlli.hash(a:longint; b:SizeUInt):SizeUInt;
begin
  hash:= a mod b;
end;

var data:maplli; i:longint;
    pair : maplli.TPair;

begin
  data:=maplli.Create;

  for i:=0 to 10 do
    data[i] := 17*i;

  data.delete(5);

  {Iteration through elements}
  // destroying class iterators is afaik a FPC extension.
  for pair in data do
    writeln(pair.Key, ' ', pair.Value);

  data.Destroy;
end.
