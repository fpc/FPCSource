{
    This file is part of the Free Pascal run time library.
    Copyright (c) 2009 by Sven Barth

    FPC Pascal system unit for the WinNT API.

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}
unit System;
interface

{$ifdef SYSTEMDEBUG}
  {$define SYSTEMEXCEPTIONDEBUG}
{$endif SYSTEMDEBUG}

{.$define FPC_HAS_INDIRECT_MAIN_INFORMATION}

{$ifdef cpui386}
  {$define Set_i386_Exception_handler}
{$endif cpui386}

{.$define DISABLE_NO_THREAD_MANAGER}

{$ifdef KMODE}
  {$define HAS_MEMORYMANAGER}
{$endif KMODE}

{ include system-independent routine headers }
{$I systemh.inc}

var
  CurrentPeb: Pointer;
  IsDeviceDriver: Boolean = False;

const
 LineEnding = #13#10;
 LFNSupport = true;
 DirectorySeparator = '\';
 DriveSeparator = '\';
 ExtensionSeparator = '.';
 PathSeparator = ';';
 AllowDirectorySeparators : set of char = ['\'];
 AllowDriveSeparators : set of char = [];

{ FileNameCaseSensitive is defined separately below!!! }
 maxExitCode = High(LongInt);
 MaxPathLen = High(Word);
 AllFilesMask = '*';

type
   PEXCEPTION_FRAME = ^TEXCEPTION_FRAME;
   TEXCEPTION_FRAME = record
     next : PEXCEPTION_FRAME;
     handler : pointer;
   end;

{$ifndef kmode}
type
  TDLL_Entry_Hook = procedure (dllparam : longint);

const
  Dll_Process_Detach_Hook : TDLL_Entry_Hook = nil;
  Dll_Thread_Attach_Hook : TDLL_Entry_Hook = nil;
  Dll_Thread_Detach_Hook : TDLL_Entry_Hook = nil;
{$endif}

const
  // NT is case sensitive
  FileNameCaseSensitive : boolean = true;
  // todo: check whether this is really the case on NT
  CtrlZMarksEOF: boolean = true; (* #26 is considered as end of file *)

  sLineBreak = LineEnding;

  { Thread count for DLL }
  Thread_count : longint = 0;
  System_exception_frame : PEXCEPTION_FRAME =nil;

implementation

{ include system independent routines }
{$I system.inc}

procedure KeQueryTickCount(TickCount: PLargeInteger); stdcall; external ntdll name 'KeQueryTickCount';

procedure randomize;
var
  tc: PLargeInteger;
begin
  FillChar(tc, SizeOf(TLargeInteger), 0);
  KeQueryTickCount(@tc);
  // the lower part should differ most on system startup
  randseed := tc^.LowPart;
end;

{*****************************************************************************
                         System Dependent Exit code
*****************************************************************************}

procedure PascalMain;stdcall;external name 'PASCALMAIN';

{$ifndef KMODE}
function NtTerminateProcess(aProcess: THandle; aStatus: NTSTATUS): NTSTATUS; stdcall; external ntdll name 'NtTerminateProcess';
{$endif KMODE}

Procedure system_exit;
begin
  if IsLibrary or IsDeviceDriver then
    Exit;
{$ifndef KMODE}
  NtTerminateProcess(THandle(-1), ExitCode);
{$endif KMODE}
end;

{$ifdef kmode}
function FPCDriverStartup(aDriverObject: Pointer; aRegistryPath: Pointer): NTSTATUS; [public, alias: 'FPC_DriverStartup'];
begin
  IsDeviceDriver := True;
  IsConsole := True;
  IsLibrary := True;

  SysDriverObject := aDriverObject;
  SysRegistryPath := aRegistryPath;

  PASCALMAIN;

  SysDriverObject := Nil;
  SysRegistryPath := Nil;

  Result := ExitCode;
end;
{$else}

const
   DLL_PROCESS_ATTACH = 1;
   DLL_THREAD_ATTACH = 2;
   DLL_PROCESS_DETACH = 0;
   DLL_THREAD_DETACH = 3;

function FPCDLLEntry(aHInstance: Pointer; aDLLReason: LongInt; aDLLParam: LongInt): LongBool; [public, alias: 'FPC_DLLEntry'];
begin
  IsLibrary := True;
  FPCDLLEntry := True;
  case aDLLReason of
    DLL_PROCESS_ATTACH: begin
      PascalMain;
      FPCDLLEntry := ExitCode = 0;
    end;
    DLL_THREAD_ATTACH: begin
      if Dll_Thread_Attach_Hook <> Nil then
        Dll_Thread_Attach_Hook(aDllParam);
    end;
    DLL_THREAD_DETACH: begin
      if Dll_Thread_Detach_Hook <> Nil then
        Dll_Thread_Detach_Hook(aDllParam);
    end;
    DLL_PROCESS_DETACH: begin
      if Dll_Process_Detach_Hook <> Nil then
        Dll_Process_Detach_Hook(aDllParam);
      // finalize units
      do_exit;
    end;
  end;
end;

procedure FPCProcessStartup(aArgument: Pointer);[public, alias: 'FPC_ProcessStartup'];
begin
  IsConsole := True;
  IsLibrary := False;
  CurrentPeb := aArgument;

  PASCALMAIN;

  system_exit;
end;
{$endif}

{$ifdef kmode}

// Kernel Mode Entry Point

function NtDriverEntry( aDriverObject: Pointer; aRegistryPath: Pointer ): LongInt; stdcall; [public, alias: '_NtDriverEntry'];
begin
  NtDriverEntry := FPCDriverStartup(aDriverObject, aRegistryPath);
end;
{$else}

// User Mode Entry Points

procedure NtProcessStartup( aArgument: Pointer ); stdcall; [public, alias: '_NtProcessStartup'];
begin
  FPCProcessStartup(aArgument);
end;

function DLLMainStartup( aHInstance: Pointer; aDLLReason, aDLLParam: LongInt ): LongBool; stdcall; [public, alias: '_DLLMainStartup'];
begin
  DLLMainStartup := FPCDLLEntry(aHInstance, aDLLReason, aDLLParam);
end;
{$endif}

begin
{$if not defined(KMODE) and not defined(HAS_MEMORYMANAGER)}
  { Setup heap }
  InitHeap;
{$endif ndef KMODE and ndef HAS_MEMORYMANAGER}
  SysInitExceptions;
  initvariantmanager;
  { we do not use winlike widestrings and also the RTL can't be compiled with
    2.2, so we can savely use the UnicodeString manager only. }
  initunicodestringmanager;
end.

