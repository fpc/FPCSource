{
    This file is part of the Free Pascal run time library.
    Copyright (c) 1999-2000 by Florian Klaempfl
    member of the Free Pascal development team

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}

unit System;

interface

{$DEFINE FPC_ANSI_TEXTFILEREC}

{$i systemh.inc}

{Platform specific information}
const
    LineEnding = #10;
    LFNSupport = false;
    DirectorySeparator = '/';
    DriveSeparator = ':';
    ExtensionSeparator = '.';
    PathSeparator = ';';
    AllowDirectorySeparators : set of char = ['\','/'];
    AllowDriveSeparators : set of char = [':'];
    FileNameCaseSensitive = false;
    FileNameCasePreserving = true;
    CtrlZMarksEOF: boolean = false; (* #26 not considered as end of file *)
    maxExitCode = 255; {.$ERROR TODO: CONFIRM THIS}
    MaxPathLen = 256;
    AllFilesMask = '*';

    sLineBreak = LineEnding;
    DefaultTextLineBreakStyle : TTextLineBreakStyle = tlbsLF;

const
    UnusedHandle    = $ffff;
    StdInputHandle  = 0;
    StdOutputHandle = 1;
    StdErrorHandle  = $ffff;

var
    args: PChar;
    argc: LongInt;
    argv: PPChar;
    envp: PPChar;


{$if defined(FPUSOFT)}

    {$define fpc_softfpu_interface}
    {$i softfpu.pp}
    {$undef fpc_softfpu_interface}

{$endif defined(FPUSOFT)}


    var
       { this variables are passed to PilotMain by the PalmOS }
       cmd : Word;
       cmdPBP : PChar; // Ptr;
       launchFlags : Word;

  implementation

{$if defined(FPUSOFT)}

    {$define fpc_softfpu_implementation}
    {$define softfpu_compiler_mul32to64}
    {$define softfpu_inline}
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

{$i system.inc}
{$i syspara.inc}

    { mimic the C start code }
    function PilotMain(_cmd : Word;_cmdPBP : PChar;{Ptr;}_launchFlags : Word) : DWord;cdecl;public;

      begin
         cmd:=_cmd;
         cmdPBP:=_cmdPBP;
         launchFlags:=_launchFlags;
//         asm
//            bsr PASCALMAIN
//         end;
         PilotMain:=ExitCode;
      end;

  procedure SysInitParamsAndEnv;
  begin
    {$WARNING: make sure argv/argc will be correct here}
    GenerateArgs;
  end;

  procedure randomize;
  begin
    {$WARNING: randseed initial value is zero!}
    randseed:=0;
  end;


{*****************************************************************************
                         System Dependent Exit code
*****************************************************************************}
Procedure system_exit;
begin
end;

function GetProcessID: SizeUInt;
begin
  GetProcessID := 1;
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

function CheckInitialStkLen(stklen : SizeUInt) : SizeUInt;
begin
  result := stklen;
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
  SysInitParamsAndEnv;
end.
