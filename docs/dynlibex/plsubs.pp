program testsubs;

uses dynlibs;

Type
  TSubStrFunc =
    function(const CString:PChar;FromPos,ToPos: longint):PChar;cdecl;

var
  s: PChar;
  FromPos, ToPos: Integer;
  lib : TLibHandle;
  SubStr : TSubStrFunc;

begin
  s := 'Test';
  FromPos := 2;
  ToPos := 3;
  lib:=LoadLibrary('libsubs.so');
  Pointer(Substr):=GetProcedureAddress(lib,'SubStr');
  WriteLn(SubStr(s, FromPos, ToPos));
  UnLoadLibrary(lib);
end.
