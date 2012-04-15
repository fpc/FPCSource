{$mode objfpc}

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

var data:maplli; i:longint; iterator:maplli.TIterator;

begin
  data:=maplli.Create;

  for i:=0 to 10 do
    data[i] := 17*i;

  data.delete(5);

  {Iteration through elements}
  iterator:=data.Iterator;
  repeat
    writeln(iterator.Key, ' ', iterator.Value);
  until not iterator.Next;
  {Don't forget to destroy iterator}
  iterator.Destroy;

  data.Destroy;
end.
