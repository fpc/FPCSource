program Project1;
uses gmap,gutil;
type
  TMyMap=specialize TMap<String,Integer,specialize TLess<string>>;
var
  Map:TMyMap;
  Pair:TMyMap.TPair;
  Count:Integer;
begin
  Map:=TMyMap.Create;
  Map.insert('test1',1);
  Map.insert('test2',2);
  count:=0;
  For Pair in Map do begin
    writeln(Pair.Key);
    inc(count);
  end;
  if count=0 then
    halt(1);
  Map.Free;
end. 
