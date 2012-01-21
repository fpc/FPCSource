{$mode objfpc}

uses ghashset;

type hashlli=class
       public
       class function hash(a:longint; b:SizeUInt):SizeUInt;
     end;
     setlli=specialize THashSet<longint, hashlli>;

class function hashlli.hash(a:longint; b:SizeUInt):SizeUInt;
begin
  hash:= a mod b;
end;

var data:setlli; i:longint; iterator:setlli.TIterator;

begin
  data:=setlli.Create;

  for i:=0 to 10 do
    data.insert(i);

  {Iteration through elements}
  iterator:=data.Iterator;
  repeat
    writeln(iterator.Data);
  until not iterator.Next;
  {Don't forget to destroy iterator}
  iterator.Destroy;

  data.Destroy;
end.
