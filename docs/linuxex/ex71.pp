program TestC{lone};

{$ifdef Linux}
// close is very Linux specific. 1.9.x threading is done via pthreads.

uses
  Linux, Errors, crt;

const
  Ready : Boolean = false;
  aChar : Char    = 'a';

function CloneProc( Arg: Pointer ): LongInt; Cdecl;
begin
  WriteLn('Hello from the clone ',PChar(Arg));
  repeat
    Write(aChar);
    Select(0,0,0,0,600);
  until Ready;
  WriteLn( 'Clone finished.');
  CloneProc := 1;
end;

var
  PID : LongInt;

procedure MainProc;
begin
  WriteLn('cloned process PID: ', PID );
  WriteLn('Press <ESC> to kill ... ' );
  repeat
    Write('.');
    Select(0,0,0,0,300);
    if KeyPressed then
      case ReadKey of
        #27: Ready := true;
        'a': aChar := 'A';
        'A': aChar := 'a';
        'b': aChar := 'b';
        'B': aChar := 'B';
      end;
  until Ready;
  WriteLn('Ready.');
end;

const
  StackSze = 16384;
  theFlags = CLONE_VM+CLONE_FS+CLONE_FILES+CLONE_SIGHAND;
  aMsg     : PChar = 'Oops !';

var
  theStack : Pointer;
  ExitStat : LongInt;

begin
  GetMem(theStack,StackSze);
  PID := Clone(@CloneProc,
               Pointer( LongInt(theStack)+StackSze),
               theFlags,
               aMsg);
  if PID < 0 then
    WriteLn('Error : ', LinuxError, ' when cloning.')
  else
    begin
    MainProc;
    case WaitPID(0,@ExitStat,Wait_Untraced or wait_clone) of
      -1: WriteLn('error:',LinuxError,'; ',StrError(LinuxError));
       0: WriteLn('error:',LinuxError,'; ',StrError(LinuxError));
    else
      WriteLn('Clone exited with: ',ExitStat shr 8);
    end;
    end;
  FreeMem( theStack, StackSze );
{$else}
begin
{$endif}
end.