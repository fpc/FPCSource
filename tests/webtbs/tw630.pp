{ Program 1 : memory waste
 dummy test }

USES SysUtils;

procedure test_it;
var
        sRec : TSearchRec;
begin
        writeln(memAvail);
        findFirst('c:\*.*',faVolumeId,sRec);
        findClose(sRec);
        writeln(sRec.name);
        writeln(memAvail);      { 288 bytes waste ! }
end;

begin
  Writeln('Before call ',MemAvail);
  test_it;
  Writeln('After call : ',MemAvail);
end.
(*{ Program 2 : correct }

USES Dos;

var
        sRec : searchRec;
begin
        writeln(memAvail);
        findFirst('c:\*.*',volumeid,sRec);
        findClose(sRec);
        writeln(sRec.name);
        writeln(memAvail);      { no memory waste ! }
end. *)
