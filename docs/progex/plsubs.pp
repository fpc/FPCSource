program testsubs;

Type
  TSubStrFunc =
    function(const CString:PChar;FromPos,ToPos: longint):PChar;cdecl;

Function dlopen(name: pchar;mode: longint):pointer;cdecl;external 'dl';
Function dlsym(lib: pointer; name: pchar):pointer;cdecl;external 'dl';
Function dlclose(lib: pointer):longint;cdecl;external 'dl';

var
  s: PChar;
  FromPos, ToPos: Integer;
  lib : pointer;
  SubStr : TSubStrFunc;

begin
  s := 'Test';
  FromPos := 2;
  ToPos := 3;
  lib:=dlopen('libsubs.so',1);
  Pointer(Substr):=dlsym(lib,'SubStr');
  WriteLn(SubStr(s, FromPos, ToPos));
  dlclose(lib);
end.
