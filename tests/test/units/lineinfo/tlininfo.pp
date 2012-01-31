{%OPT=-gl -O-}
{%TARGET=os2,go32v2,win32}
program tlininfo;

{ $DEFINE DEBUG}

procedure TestProc;
begin
 WriteLn ('TestProc running...');
end;

var
 Success: boolean;
 Func, Src, SrcFName: string;
 Line: longint;

begin
 TestProc;
 Success := GetLineInfo (PtrUInt (@TestProc) + 1, Func, Src, Line);
 SrcFName := Copy (Src, Length (Src) - (Length ('tlininfo.pp') - 1),
                                                       Length ('tlininfo.pp'));
{$IFDEF DEBUG}
 WriteLn ('Source address = ', HexStr (@TestProc));
 WriteLn ('Success (GetLineInfo call) = ', Success);
 WriteLn ('Func = ''', Func, ''' (expected ''TESTPROC'')');
 WriteLn ('Src = ', Src);
 WriteLn ('Filename part of source path = ''',
                                    SrcFName, ''' (expected ''tlininfo.pp'')');
 WriteLn ('Line = ', Line, ' (expected 8)');
{$ENDIF DEBUG}
 if not (Success and (SrcFName = 'tlininfo.pp') and
                                       (Func = 'TESTPROC') and (Line = 8)) then
  begin
   WriteLn ('Failed!');
{$IFNDEF DEBUG}
   WriteLn ('Success of GetLineInfo call = ', Success);
   WriteLn ('Func = ''', Func, ''' (expected ''TESTPROC'')');
   WriteLn ('Src = ', Src);
   WriteLn ('Filename part of source path = ''',
                                    SrcFName, ''' (expected ''tlininfo.pp'')');
   WriteLn ('Line = ', Line, ' (expected 8)');
{$ENDIF DEBUG}
   Halt (1);
  end
{$IFDEF DEBUG}
 else
  WriteLn ('Test successful.')
{$ENDIF DEBUG}
end.
