{ Program 1 : memory waste
 dummy test }

USES SysUtils,erroru;

procedure test_it;
var
        sRec : TSearchRec;
begin
        findFirst('c:\*.*',faVolumeId,sRec);
        findClose(sRec);
        writeln(sRec.name);
end;

var
  mem : sizeint;
begin
  mem:=0;
  DoMem(mem);
  test_it;
  DoMem(mem);
end.
