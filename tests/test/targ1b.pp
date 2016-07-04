{%skiptarget=wince}
{%delfiles=targ1a}

{ This file is to check if there is some memory corruption
  due to startup code with argument loading
  go32v2 target had this problem
  close to 2.2 release 2007-03-27 pierre }

program create_startup_test_crash;

{$ifdef go32v2}
{$define HasExeSuffix}
{$endif}
{$ifdef msdos}
{$define HasExeSuffix}
{$define DosMaxArg}
{$endif}
{$ifdef win32}
{$define HasExeSuffix}
{$endif}
{$ifdef win64}
{$define HasExeSuffix}
{$endif}
{$ifdef wince}
{$define HasExeSuffix}
{$endif}
{$ifdef os2}
{$define HasExeSuffix}
{$endif}
{$ifdef emx}
{$define HasExeSuffix}
{$endif}
{$ifdef wdosx}
{$define HasExeSuffix}
{$endif}
{$ifdef netware}
{$define HasNlmSuffix}
{$endif}
{$ifdef netwlibc}
{$define HasNlmSuffix}
{$endif}

uses
  dos;

const
  Prefix =
{$ifdef Unix}
  './'
{$else}
  ''
{$endif}
  ;
  ExeSuffix =
{$ifdef HasExeSuffix}
  '.exe'
{$else}
  {$ifdef HasNlmSuffix}
    '.nlm'
  {$else}
    ''
  {$endif}
{$endif}
  ;
const
  MAX = 255;

{$ifdef DosMaxArg}
  OK_L = 126;
{$else not DosMaxArg}
  OK_L = 200;
{$endif not DosMaxArg}

var
  cmd,
  arg,arg2_length_string,args : string;
  i, first, first_wrong,arg2_length,total_length : longint;
const
  Everything_ok : boolean = true;
begin
  cmd:=Prefix+'targ1a'+ExeSuffix;
  arg:='';
  first_wrong:=-1;
  if paramcount>0 then
    begin
      val(paramstr(1),first);
      for i:=1 to first do
        arg:=arg+'a';
    end
  else
    first:=0;
  for i:=first to MAX - 4 { 4 chars needed for first arg and space } do
    begin
      arg2_length:=length(arg);
      Writeln(stderr,'Going to call "',cmd,'" with arg2 = "',arg,'"');
      Writeln(stderr,'arg2 length =',arg2_length);
      str(arg2_length, arg2_length_string);
      args:=arg2_length_string+' '+arg;
      total_length:=length(args);
      Writeln(stderr,'Length of all args=',total_length);
      Exec(cmd,args);
      if (DosExitCode<>0) or (DosError<>0) then
        begin
          if DosError<>0 then
            Writeln(stderr,'Dos.Exec error detected, DosError=', DosError)
          else
            Writeln(stderr,'Error running targ1a, DosExitCode=',DosExitCode);
          if first_wrong=-1 then
            first_wrong:=total_length;
          Everything_ok := false;
          break;
        end;
      arg:=arg+'a';
    end;
  if Everything_ok then
    begin
      Writeln(stderr,'Test successful: no memory corruption occurs');
    end
  else
    begin
      Writeln(stderr,'Test fails: Memory corruption occurs');
      Writeln(stderr,'First arg length where error appears is ',first_wrong);
      if first_wrong<OK_L then
        RunError(1)
      else
        Writeln(stderr,'Warning: when using Dos.Exec, arg length must be smaller than ',first_wrong);
    end;
end.

