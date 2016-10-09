{
    This file is part of the Free Pascal run time library.
    Copyright (c) 2016 the Free Pascal development team

    Portions based on the Atari RTL for FPC 1.x
    Copyright (c) 1999-2000 by Carl Eric Codere
    member of the Free Pascal development team

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}
unit System;

{--------------------------------------------------------------------}
{ LEFT TO DO:                                                        }
{--------------------------------------------------------------------}
{ o SBrk                                                             }
{ o Implement truncate                                               }
{ o Implement paramstr(0)                                            }
{--------------------------------------------------------------------}


  interface

    {$I systemh.inc}

{Platform specific information}
const
    LineEnding = #13#10;
    LFNSupport = false;
    CtrlZMarksEOF: boolean = false; (* #26 not considered as end of file *)
    DirectorySeparator = '/';
    DriveSeparator = ':';
    ExtensionSeparator = '.';
    PathSeparator = ';';
    AllowDirectorySeparators : set of char = ['\','/'];
    AllowDriveSeparators : set of char = [':'];
    FileNameCaseSensitive = false;
    FileNameCasePreserving = false;
    maxExitCode = 255;
    MaxPathLen = 255;
    AllFilesMask = '*';

    sLineBreak = LineEnding;
    DefaultTextLineBreakStyle : TTextLineBreakStyle = tlbsCRLF;

const
    UnusedHandle    = $ffff;
    StdInputHandle  = 0;
    StdOutputHandle = 1;
    StdErrorHandle  = $ffff;

var
    args: Pointer; external name '__ARGS'; { Defined in the startup code }
    argc: LongInt;
    argv: PPChar;
    envp: PPChar;


    {$if defined(FPUSOFT)}

    {$define fpc_softfpu_interface}
    {$i softfpu.pp}
    {$undef fpc_softfpu_interface}

    {$endif defined(FPUSOFT)}


  implementation

    {$if defined(FPUSOFT)}

    {$define fpc_softfpu_implementation}
    {$i softfpu.pp}
    {$undef fpc_softfpu_implementation}

    { we get these functions and types from the softfpu code }
    {$define FPC_SYSTEM_HAS_float64}
    {$define FPC_SYSTEM_HAS_float32}
    {$define FPC_SYSTEM_HAS_flag}
    {$define FPC_SYSTEM_HAS_extractFloat64Frac0}
    {$define FPC_SYSTEM_HAS_extractFloat64Frac1}
    {$define FPC_SYSTEM_HAS_extractFloat64Exp}
    {$define FPC_SYSTEM_HAS_extractFloat64Sign}
    {$define FPC_SYSTEM_HAS_ExtractFloat32Frac}
    {$define FPC_SYSTEM_HAS_extractFloat32Exp}
    {$define FPC_SYSTEM_HAS_extractFloat32Sign}

    {$endif defined(FPUSOFT)}

    {$I system.inc}

function GetProcessID:SizeUInt;
begin
  {$WARNING To be checked by platform maintainer}
   GetProcessID := 1;
end;



{$S-}
(*    procedure Stack_Check; assembler;
    { Check for local variable allocation }
    { On Entry -> d0 : size of local stack we are trying to allocate }
         asm
          XDEF STACKCHECK
           move.l  sp,d1            { get value of stack pointer            }
           sub.l   d0,d1            {  sp - stack_size                      }
           sub.l   #2048,d1
           cmp.l   __BREAK,d1
           bgt     @st1nosweat
           move.l  #202,d0
           jsr     HALT_ERROR
         @st1nosweat:
         end;*)


   Function GetParamCount(const p: pchar): longint;
   var
    i: word;
    count: word;
   Begin
    i:=0;
    count:=0;
    while p[count] <> #0 do
     Begin
       if (p[count] <> ' ') and (p[count] <> #9) and (p[count] <> #0) then
       Begin
          i:=i+1;
          while (p[count] <> ' ') and (p[count] <> #9) and (p[count] <> #0) do
           count:=count+1;
       end;
       if p[count] = #0 then break;
       count:=count+1;
     end;
     GetParamCount:=longint(i);
   end;


   Function GetParam(index: word; const p : pchar): string;
   { On Entry: index = string index to correct parameter  }
   { On exit:  = correct character index into pchar array }
   { Returns correct index to command line argument }
   var
    count: word;
    localindex: word;
    l: byte;
    temp: string;
   Begin
     temp:='';
     count := 0;
     { first index is one }
     localindex := 1;
     l:=0;
     While p[count] <> #0 do
       Begin
         if (p[count] <> ' ') and (p[count] <> #9) then
           Begin
             if localindex = index then
              Begin
               while (p[count] <> #0) and (p[count] <> ' ') and (p[count] <> #9) and (l < 256) do
                Begin
                  temp:=temp+p[count];
                  l:=l+1;
                  count:=count+1;
                end;
                temp[0]:=char(l);
                GetParam:=temp;
                exit;
              end;
             { Point to next argument in list }
             while (p[count] <> #0) and (p[count] <> ' ') and (p[count] <> #9) do
               Begin
                 count:=count+1;
               end;
             localindex:=localindex+1;
           end;
         if p[count] = #0 then break;
         count:=count+1;
       end;
     GetParam:=temp;
   end;


    function paramstr(l : longint) : string;
      var
       p : pchar;
       s1 : string;
      begin
         if l = 0 then
         Begin
           s1 := '';
         end
         else
         if (l>0) and (l<=paramcount) then
           begin
             p:=args;
             paramstr:=GetParam(word(l),p);
           end
         else paramstr:='';
      end;

      function paramcount : longint;
      Begin
        paramcount := argc;
      end;


  { This routine is used to grow the heap.  }
  { But here we do a trick, we say that the }
  { heap cannot be regrown!                 }
  function sbrk( size: longint): pointer;
  { on exit nil = if fails.               }
  Begin
   sbrk:=nil;
  end;


  procedure randomize;
  begin
    {$WARNING: randseed initial value is 24bit}
    randseed:=xbios_random;
  end;

{*****************************************************************************
                         System Dependent Exit code
*****************************************************************************}
Procedure system_exit;
begin
  gemdos_pterm(ExitCode);
end;

{*****************************************************************************
                         SystemUnit Initialization
*****************************************************************************}

procedure SysInitStdIO;
begin
  OpenStdIO(Input,fmInput,StdInputHandle);
  OpenStdIO(Output,fmOutput,StdOutputHandle);
  OpenStdIO(StdOut,fmOutput,StdOutputHandle);

  OpenStdIO(StdErr,fmOutput,StdErrorHandle);
  OpenStdIO(ErrOutput,fmOutput,StdErrorHandle);
end;

function CheckInitialStkLen (StkLen: SizeUInt): SizeUInt;
begin
  CheckInitialStkLen := StkLen;
end;


begin
  StackLength := CheckInitialStkLen (InitialStkLen);
{ Initialize ExitProc }
  ExitProc:=Nil;
{ Setup heap }
  InitHeap;
  SysInitExceptions;
  InitUnicodeStringManager;
{ Setup stdin, stdout and stderr }
  SysInitStdIO;
{ Reset IO Error }
  InOutRes:=0;
{ Setup command line arguments }
//  argc:=GetParamCount(args);
{$ifdef FPC_HAS_FEATURE_THREADING}
  InitSystemThreads;
{$endif}
end.
