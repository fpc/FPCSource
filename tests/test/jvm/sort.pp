program sort;

{$mode delphi}
{$modeswitch unicodestrings}

uses
  {$ifdef java}jdk15{$else}androidr14{$endif};

function test : string;
var
 sa : array of JLObject;
 L : JUList;
 i : integer;
begin
 SetLength(sa, 3);
 sa[0] := JLString(string('2'));
 sa[1] := JLString(string('3'));
 sa[2] := JLString(string('1'));
 L := JUArrays.asList(sa);
 JUCollections.sort(L);

 Result := '';
 for i := 0 to L.size() - 1 do
   Result := Result + string(L.get(i)) + string(' ');
end;

begin
  jlsystem.fout.println(test);
  if test<>'1 2 3 ' then
    raise JLException.create;
end.
