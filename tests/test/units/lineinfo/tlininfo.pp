{%OPT=-gl -o}
{%TARGET=os2,go32v2,win32}
program tlininfo;

{ $DEFINE DEBUG}

procedure TESTPROC;
begin
 WriteLn ('TestProc running...');
end;

var
 Success: boolean;
 Func, Src: string;
 Line: longint;

begin
 TestProc;
 Success := GetLineInfo (PtrUInt (@TestProc) + 4, Func, Src, Line);
{$IFDEF DEBUG}
 WriteLn ('Source address = ', HexStr (@TestProc));
 WriteLn ('Success = ', Success);
 WriteLn ('Func = ', Func);
 WriteLn ('Src = ', Src);
 WriteLn ('Line = ', Line);
{$ENDIF DEBUG}
 if not (Success and (Copy (Src, Length (Src) - 11, 12) = 'tlininfo.pas') and
                                       (Func = 'TESTPROC') and (Line = 8)) then
  Halt (1)
end.
