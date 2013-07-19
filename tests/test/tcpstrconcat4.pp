{$mode delphiunicode}

type
  tstr850 = type ansistring(850);
  tstr866 = type ansistring(866);
  tstr65001 = type ansistring(65001);

procedure test;
var
  s1: tstr850;
  s2: tstr866;
  s3: tstr65001;
  r: rawbytestring;
begin
  s1:='a';
  s2:='b';
  s3:='c';
  r:='d';
  r:=s1+s2;
  writeln(stringcodepage(r));
  if (stringcodepage(r)<>0) and
     (stringcodepage(r)<>defaultsystemcodepage) then
    halt(1);
  setcodepage(r,850);
  r:=s1+s2;
  writeln(stringcodepage(r));
  if (stringcodepage(r)<>0) and
     (stringcodepage(r)<>defaultsystemcodepage) then
    halt(2);
  setcodepage(r,CP_ASCII);
  r:=s1+s2;
  writeln(stringcodepage(r));
  if (stringcodepage(r)<>0) and
     (stringcodepage(r)<>defaultsystemcodepage) then
    halt(3);
  r:=s1+s1;
  writeln(stringcodepage(r));
  if (stringcodepage(r)<>stringcodepage(s1)) then
    halt(4);
  r:=s2+s2;
  writeln(stringcodepage(r));
  if (stringcodepage(r)<>stringcodepage(s2)) then
    halt(5);
end;

begin
  test;
end.

