{
    $Id$
    This file is part of the Free Pascal run time library.
    Copyright (c) 2004 by Karoly Balogh for Genesi S.a.r.l.

    System unit for MorphOS/PowerPC

    Uses parts of the Commodore Amiga/68k port by Carl Eric Codere
    and Nils Sjoholm

    MorphOS port was done on a free Pegasos II/G4 machine
    provided by Genesi S.a.r.l. <www.genesi.lu>

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}

unit {$ifdef VER1_0}SysMorph{$else}System{$endif};

interface

{$define FPC_IS_SYSTEM}

{$I systemh.inc}

const
  LineEnding = #10;
  LFNSupport = True;
  DirectorySeparator = '/';
  DriveSeparator = ':';
  PathSeparator = ';';
  maxExitCode = 255;

const
  UnusedHandle    : LongInt = -1;
  StdInputHandle  : LongInt = 0;
  StdOutputHandle : LongInt = 0;
  StdErrorHandle  : LongInt = 0;

  FileNameCaseSensitive : Boolean = False;

  sLineBreak : string[1] = LineEnding;
  DefaultTextLineBreakStyle : TTextLineBreakStyle = tlbsLF;

  BreakOn : Boolean = True;


var
  MOS_ExecBase   : Pointer; external name '_ExecBase';
  MOS_DOSBase    : Pointer;
  MOS_UtilityBase: Pointer;

  MOS_heapPool : Pointer; { pointer for the OS pool for growing the heap }
  MOS_origDir  : LongInt; { original directory on startup }
  MOS_ambMsg   : Pointer;
  MOS_ConName  : PChar ='CON:10/30/620/100/FPC Console Output/AUTO/CLOSE/WAIT';
  MOS_ConHandle: LongInt;

  argc: LongInt;
  argv: PPChar;
  envp: PPChar;


implementation

{$I system.inc}


{*****************************************************************************
                       Misc. System Dependent Functions
*****************************************************************************}

procedure haltproc(e:longint);cdecl;external name '_haltproc';

procedure System_exit;
begin
  { We must remove the CTRL-C FLAG here because halt }
  { may call I/O routines, which in turn might call  }
  { halt, so a recursive stack crash                 }
  if BreakOn then begin
    if (SetSignal(0,0) and SIGBREAKF_CTRL_C)<>0 then
      SetSignal(0,SIGBREAKF_CTRL_C);
  end;

  { Closing opened files }
  CloseList(MOS_fileList);

  { Changing back to original directory if changed }
  if MOS_origDir<>0 then begin
    CurrentDir(MOS_origDir);
  end;

  if MOS_UtilityBase<>nil then CloseLibrary(MOS_UtilityBase);
  if MOS_DOSBase<>nil then CloseLibrary(MOS_DOSBase);
  if MOS_heapPool<>nil then DeletePool(MOS_heapPool);
  haltproc(ExitCode);
end;

{ Generates correct argument array on startup }
procedure GenerateArgs;
var
  argvlen : longint;

  procedure allocarg(idx,len:longint);
    var
      i,oldargvlen : longint;
    begin
      if idx>=argvlen then
        begin
          oldargvlen:=argvlen;
          argvlen:=(idx+8) and (not 7);
          sysreallocmem(argv,argvlen*sizeof(pointer));
          for i:=oldargvlen to argvlen-1 do
            argv[i]:=nil;
        end;
      { use realloc to reuse already existing memory }
      sysreallocmem(argv[idx],len+1);
    end;

var
  count: word;
  start: word;
  localindex: word;
  p : pchar;
  temp : string;

begin
  p:=GetArgStr;
  argvlen:=0;

  { Set argv[0] }
  temp:=paramstr(0);
  allocarg(0,length(temp));
  move(temp[1],argv[0]^,length(temp));
  argv[0][length(temp)]:=#0;

  { check if we're started from Ambient }
  if MOS_ambMsg<>nil then
    begin
      argc:=0;
      exit;
    end;

  { Handle the other args }
  count:=0;
  { first index is one }
  localindex:=1;
  while (p[count]<>#0) do
    begin
      while (p[count]=' ') or (p[count]=#9) or (p[count]=LineEnding) do inc(count);
      start:=count;
      while (p[count]<>#0) and (p[count]<>' ') and (p[count]<>#9) and (p[count]<>LineEnding) do inc(count);
      if (count-start>0) then
        begin
          allocarg(localindex,count-start);
          move(p[start],argv[localindex]^,count-start);
          argv[localindex][count-start]:=#0;
          inc(localindex);
        end;
    end;
  argc:=localindex;
end;

function GetProgDir: String;
var
  s1     : String;
  alock  : LongInt;
  counter: Byte;
begin
  GetProgDir:='';
  FillChar(s1,255,#0);
  { GetLock of program directory }
  alock:=GetProgramDir;
  if alock<>0 then begin
    if NameFromLock(alock,@s1[1],255) then begin
      counter:=1;
      while (s1[counter]<>#0) and (counter<>0) do Inc(counter);
      s1[0]:=Char(counter-1);
      GetProgDir:=s1;
    end;
  end;
end;

function GetProgramName: String;
{ Returns ONLY the program name }
var
  s1     : String;
  counter: Byte;
begin
  GetProgramName:='';
  FillChar(s1,255,#0);
  if GetProgramName(@s1[1],255) then begin
    { now check out and assign the length of the string }
    counter := 1;
    while (s1[counter]<>#0) and (counter<>0) do Inc(counter);
    s1[0]:=Char(counter-1);

    { now remove any component path which should not be there }
    for counter:=length(s1) downto 1 do
      if (s1[counter] = '/') or (s1[counter] = ':') then break;
    { readjust counterv to point to character }
    if counter<>1 then Inc(counter);

    GetProgramName:=copy(s1,counter,length(s1));
  end;
end;


{*****************************************************************************
                             ParamStr/Randomize
*****************************************************************************}

{ number of args }
function paramcount : longint;
begin
  if MOS_ambMsg<>nil then
    paramcount:=0
  else
    paramcount:=argc-1;
end;

{ argument number l }
function paramstr(l : longint) : string;
var
  s1: String;
begin
  paramstr:='';
  if MOS_ambMsg<>nil then exit;

  if l=0 then begin
    s1:=GetProgDir;
    if s1[length(s1)]=':' then paramstr:=s1+GetProgramName
                          else paramstr:=s1+'/'+GetProgramName;
  end else begin
    if (l>0) and (l+1<=argc) then paramstr:=strpas(argv[l]);
  end;
end;

{ set randseed to a new pseudo random value }
procedure randomize;
var tmpTime: TDateStamp;
begin
  DateStamp(@tmpTime);
  randseed:=tmpTime.ds_tick;
end;


{ MorphOS specific startup }
procedure SysInitMorphOS;
var self: PProcess;
begin
 self:=PProcess(FindTask(nil));
 if self^.pr_CLI=0 then begin
   { if we're running from Ambient/Workbench, we catch its message }
   WaitPort(@self^.pr_MsgPort);
   MOS_ambMsg:=GetMsg(@self^.pr_MsgPort);
 end;

 MOS_DOSBase:=OpenLibrary('dos.library',50);
 if MOS_DOSBase=nil then Halt(1);
 MOS_UtilityBase:=OpenLibrary('utility.library',50);
 if MOS_UtilityBase=nil then Halt(1);

 { Creating the memory pool for growing heap }
 MOS_heapPool:=CreatePool(MEMF_FAST,growheapsize2,growheapsize1);
 if MOS_heapPool=nil then Halt(1);

 if MOS_ambMsg=nil then begin
   StdInputHandle:=dosInput;
   StdOutputHandle:=dosOutput;
 end else begin
   MOS_ConHandle:=Open(MOS_ConName,MODE_OLDFILE);
   if MOS_ConHandle<>0 then begin
     StdInputHandle:=MOS_ConHandle;
     StdOutputHandle:=MOS_ConHandle;
   end else
     Halt(1);
 end;
end;


procedure SysInitStdIO;
begin
  OpenStdIO(Input,fmInput,StdInputHandle);
  OpenStdIO(Output,fmOutput,StdOutputHandle);
  OpenStdIO(StdOut,fmOutput,StdOutputHandle);

  { * MorphOS doesn't have a separate stderr, just like AmigaOS (???) * }
  StdErrorHandle:=StdOutputHandle;
  // OpenStdIO(StdErr,fmOutput,StdErrorHandle);
  // OpenStdIO(ErrOutput,fmOutput,StdErrorHandle);
end;

function GetProcessID: SizeUInt;
begin
 GetProcessID:=SizeUInt(FindTask(NIL)); 
end;


begin
  IsConsole := TRUE;
  IsLibrary := FALSE;
  StackLength := InitialStkLen;
  StackBottom := Sptr - StackLength;
{ OS specific startup }
  MOS_ambMsg:=nil;
  MOS_origDir:=0;
  MOS_fileList:=nil;
  envp:=nil;
  SysInitMorphOS;
{ Set up signals handlers }
//  InstallSignals;
{ Setup heap }
  InitHeap;
  SysInitExceptions;
{ Setup stdin, stdout and stderr }
  SysInitStdIO;
{ Reset IO Error }
  InOutRes:=0;
{ Arguments }
  GenerateArgs;
  InitSystemThreads;
{$ifdef HASVARIANT}
  initvariantmanager;
{$endif HASVARIANT}
{$ifdef HASWIDESTRING}
  initwidestringmanager;
{$endif HASWIDESTRING}
end.

{
  $Log$
  Revision 1.31  2005-02-07 21:30:12  peter
    * system unit updated

  Revision 1.30  2005/02/01 20:22:49  florian
    * improved widestring infrastructure manager

  Revision 1.29  2005/01/12 08:03:42  karoly
    * Few more Sysutils functions implemented

  Revision 1.28  2005/01/11 17:43:14  karoly
    * some cleanup, more sanity checks and updates for sysutils

  Revision 1.27  2004/12/14 21:01:16  karoly
    * GetProcessID implemented

  Revision 1.26  2004/12/07 10:07:50  karoly
    * removed debug code accidentally left in

  Revision 1.25  2004/12/07 09:55:46  karoly
    * previous change broke PathConv, fixed

  Revision 1.24  2004/12/06 20:09:55  karoly
    * added a public alias to PathConv for use in DOS unit

  Revision 1.23  2004/12/05 14:36:37  hajny
    + GetProcessID added

  Revision 1.22  2004/11/15 23:18:16  karoly
   * Reworked path handling to be less messy

  Revision 1.21  2004/11/04 09:32:31  peter
  ErrOutput added

  Revision 1.20  2004/10/25 15:38:59  peter
    * compiler defined HEAP and HEAPSIZE removed

  Revision 1.19  2004/09/03 19:26:15  olle
    + added maxExitCode to all System.pp
    * constrained error code to be below maxExitCode in RunError et. al.

  Revision 1.18  2004/08/09 00:12:40  karoly
    * changes to work with updated doslib includes

  Revision 1.17  2004/08/03 15:59:41  karoly
    * more cleanup & more includes

  Revision 1.16  2004/06/26 20:48:24  karoly
    * more cleanup + changes to use new includes

  Revision 1.15  2004/06/23 13:27:32  karoly
    * fixed system unit for the new heap manager

  Revision 1.14  2004/06/17 16:16:14  peter
    * New heapmanager that releases memory back to the OS, donated
      by Micha Nelissen

  Revision 1.13  2004/06/13 22:50:47  karoly
    * cleanup and changes to use new includes

  Revision 1.12  2004/06/06 23:31:13  karoly
    * fixed dos_UnLockDosList from being nonsense, and some cleanup

  Revision 1.11  2004/06/06 19:18:05  karoly
    + added support for paramstr(0)

  Revision 1.10  2004/06/05 19:49:19  karoly
    + added console I/O support when running from Ambient

  Revision 1.9  2004/05/12 23:18:54  karoly
    * fixed do_read and dos_Read from being nonsense

  Revision 1.8  2004/05/12 20:26:04  karoly
    + added syscalls and structures necessary for DOS unit

  Revision 1.7  2004/05/12 15:34:16  karoly
    * fixed startup code from endless wait when not started from Ambient

  Revision 1.6  2004/05/09 14:42:59  karoly
    * again, few more new things added

  Revision 1.5  2004/05/09 02:02:42  karoly
    * more things got implemented

  Revision 1.4  2004/05/02 02:06:57  karoly
    + most of file I/O calls implemented

  Revision 1.3  2004/05/01 15:09:47  karoly
    * first working system unit (very limited yet)

  Revision 1.2  2004/04/08 06:28:29  karoly
    * first steps to have a morphos system unit

  Revision 1.1  2004/02/13 07:19:53  karoly
    * quick hack from Linux system unit
}
