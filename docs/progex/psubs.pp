program testsubs;

function SubStr(const CString: PChar; FromPos, ToPos: longint): PChar;
  cdecl; external 'subs';

var
  s: PChar;
  FromPos, ToPos: Integer;
begin
  s := 'Test';
  FromPos := 2;
  ToPos := 3;
  WriteLn(SubStr(s, FromPos, ToPos));
end.
