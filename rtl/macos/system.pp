{
    $Id$
    This file is part of the Free Pascal run time library.
    Copyright (c) 2002 by Olle Raab

    FreePascal system unit for MacOS.

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}
unit System;

interface

{If MAC_SYS_RUNABLE is defined, this file can be included in a
 runnable program, but it then lacks lot of features. If not defined
 it tries to be faithful to a real system.pp, but it may not be
 able to assemble and link. The switch is only temporary, and only for
 use when system.pp is developed.}

{$Y-}

{$ifdef MAC_SYS_RUNABLE}

type
   integer = -32768 .. 32767;
   byte =0..255;
   shortint=-128..127;
   word=0..65535;
   longint=+(-$7FFFFFFF-1)..$7FFFFFFF;
   pchar=^char;

{$else}

{$I systemh.inc}

{$I heaph.inc}


{Platform specific information}
const
 LineEnding = #13;
 LFNSupport = true;
 DirectorySeparator = ':';
 DriveSeparator = ':';
 PathSeparator = ';';
 FileNameCaseSensitive = false;

const
  UnusedHandle    = -1;
  StdInputHandle  = 0;
  StdOutputHandle = 0;
  StdErrorHandle  = 0;

  sLineBreak : string[1] = LineEnding;
  DefaultTextLineBreakStyle : TTextLineBreakStyle = tlbsCR;

var
  argc : longint;
  argv : ppchar;
  envp : ppchar;

{$endif}

implementation

{$ifdef MAC_SYS_RUNABLE}

procedure do_exit;[public,alias:'FPC_DO_EXIT'];

begin
end;

procedure fpc_initializeunits;[public,alias:'FPC_INITIALIZEUNITS'];

begin
end;

{$else}

{$I system.inc}

{*********************** ??????? *************}

procedure SysInitStdIO;
begin
end;

{*****************************************************************************}

procedure setup_arguments;
begin
end;

procedure setup_environment;
begin
end;

{*****************************************************************************
                         System Dependent Exit code
*****************************************************************************}
Procedure system_exit;
begin
end;


{*****************************************************************************
                              ParamStr/Randomize
*****************************************************************************}

{ number of args }
function paramcount : longint;
begin
  {paramcount := argc - 1;}
  paramcount:=0;
end;

{ argument number l }
function paramstr(l : longint) : string;
begin
  {if (l>=0) and (l+1<=argc) then
   paramstr:=strpas(argv[l])
  else}
   paramstr:='';
end;

{ set randseed to a new pseudo random value }
procedure randomize;
begin
  {regs.realeax:=$2c00;
  sysrealintr($21,regs);
  hl:=regs.realedx and $ffff;
  randseed:=hl*$10000+ (regs.realecx and $ffff);}
  randseed:=0;
end;

{*****************************************************************************
                              Heap Management
*****************************************************************************}

{ first address of heap }
function getheapstart:pointer;
begin
   getheapstart:=0;
end;

{ current length of heap }
function getheapsize:longint;
begin
   getheapsize:=0;
end;

{ function to allocate size bytes more for the program }
{ must return the first address of new data space or -1 if fail }
function Sbrk(size : longint):longint;
begin
  Sbrk:=-1;
end;

{$I heap.inc}

{****************************************************************************
                        Low level File Routines
       All these functions can set InOutRes on errors
 ****************************************************************************}

{ close a file from the handle value }
procedure do_close(handle : longint);
begin
  InOutRes:=1;
end;

procedure do_erase(p : pchar);
begin
  InOutRes:=1;
end;

procedure do_rename(p1,p2 : pchar);
begin
  InOutRes:=1;
end;

function do_write(h,addr,len : longint) : longint;
begin
  InOutRes:=1;
end;

function do_read(h,addr,len : longint) : longint;
begin
  InOutRes:=1;
end;

function do_filepos(handle : longint) : longint;
begin
  InOutRes:=1;
end;

procedure do_seek(handle,pos : longint);
begin
  InOutRes:=1;
end;

function do_seekend(handle:longint):longint;
begin
  InOutRes:=1;
end;

function do_filesize(handle : longint) : longint;
begin
  InOutRes:=1;
end;

{ truncate at a given position }
procedure do_truncate (handle,pos:longint);
begin
  InOutRes:=1;
end;

procedure do_open(var f;p:pchar;flags:longint);
{
  filerec and textrec have both handle and mode as the first items so
  they could use the same routine for opening/creating.
  when (flags and $10)   the file will be append
  when (flags and $100)  the file will be truncate/rewritten
  when (flags and $1000) there is no check for close (needed for textfiles)
}
begin
  InOutRes:=1;
end;

function do_isdevice(handle:longint):boolean;
begin
  do_isdevice:=false;
end;


{*****************************************************************************
                           UnTyped File Handling
*****************************************************************************}

{$i file.inc}

{*****************************************************************************
                           Typed File Handling
*****************************************************************************}

{$i typefile.inc}

{*****************************************************************************
                           Text File Handling
*****************************************************************************}

{ should we consider #26 as the  end of a file ? }
{?? $DEFINE EOF_CTRLZ}

{$i text.inc}

{*****************************************************************************
                           Directory Handling
*****************************************************************************}
procedure mkdir(const s : string);[IOCheck];
begin
  InOutRes:=1;
end;

procedure rmdir(const s : string);[IOCheck];
begin
  InOutRes:=1;
end;

procedure chdir(const s : string);[IOCheck];
begin
  InOutRes:=1;
end;

procedure GetDir (DriveNr: byte; var Dir: ShortString);

begin
  InOutRes := 1;
end;

{*****************************************************************************
                         SystemUnit Initialization
*****************************************************************************}

Begin
  { To be set if this is a GUI or console application }
  IsConsole := TRUE;
  { To be set if this is a library and not a program  }
  IsLibrary := FALSE;
  StackBottom := SPtr - StackLength;
  ExitCode := 0;
{ Setup heap }
  InitHeap;
{ Setup stdin, stdout and stderr }
  OpenStdIO(Input,fmInput,StdInputHandle);
  OpenStdIO(Output,fmOutput,StdOutputHandle);
  OpenStdIO(StdOut,fmOutput,StdOutputHandle);
  OpenStdIO(StdErr,fmOutput,StdErrorHandle);
{ Setup environment and arguments }
  Setup_Environment;
  Setup_Arguments;
{ Reset IO Error }
  InOutRes:=0;

{$endif}

End.


{
  $Log$
  Revision 1.3  2002-10-23 15:29:09  olle
    + added switch MAC_SYS_RUNABLE
    + added include of system.h etc
    + added standard globals
    + added dummy hook procedures

  Revision 1.2  2002/10/10 19:44:05  florian
    * changes from Olle to compile/link a simple program

  Revision 1.1  2002/10/02 21:34:31  florian
    * first dummy implementation
}