{
    This file is part of the Free Pascal run time library.
    Copyright (c) 2017 by the Free Pascal development team

    System Entry point for PalmOS, Pascal only programs

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}
{$SMARTLINK OFF}
unit si_prc;

interface

implementation

{$i palmos.inc}

var
  appInfo: SysAppInfoPtr; public name '__appInfo';
  StkLen: LongInt; external name '__stklen';
  sysinit_jmpbuf: jmp_buf;
  ExitCode: LongInt;

var
  { this is declared by the PalmOS linker script }
  data_start: pbyte; external name 'data_start';


procedure PascalMain; external name 'PASCALMAIN';
procedure FPCRelocateData; forward;

{ this function must be the first in this unit which contains code }
function _FPC_proc_start: longint; cdecl; public name '_start';
var
  locAppInfo: SysAppInfoPtr;
  prevGlobals: Pointer;
  globalsPtr: Pointer;
begin
  _FPC_proc_start:=0;

  if SysAppStartup(locAppInfo, prevGlobals, globalsPtr) <> 0 then
    begin
      SndPlaySystemSound(sndError);
      exit(-1);
    end;

  if (locAppInfo^.launchFlags and sysAppLaunchFlagNewGlobals) > 0 then
    FPCRelocateData;

  { we don't support anything but normal startup now }
  { FIXME: figure it out how various startup commands can }
  { coexist with the normal system unit infrastructure (KB) }
  if locAppInfo^.cmd = sysAppLaunchCmdNormalLaunch then
    begin
      if setjmp(sysinit_jmpbuf) = 0 then
        begin
          appInfo:=locAppInfo;
          PascalMain;
        end;
      _FPC_proc_start:=ExitCode;
    end;

  SysAppExit(locAppInfo, prevGlobals, globalsPtr);
end;

procedure _FPC_proc_halt(_ExitCode: longint); cdecl; public name '_haltproc';
begin
  ExitCode:=_ExitCode;
  longjmp(sysinit_jmpbuf,1);
end;

{ data segment relocation magic, ported to Pascal from prc-tools C version }
type
  PReloc = ^TReloc;
  TReloc = record
    case boolean of
      true: ( next: smallint; addend: word );
      false: ( value: dword );
  end;

procedure RelocateChain(offset: smallint; base: pointer);
var
  data_res: pbyte;
  site: PReloc;
begin
  data_res:=@data_start;

  while offset >= 0 do
    begin
      site:=PReloc(data_res + offset);
      offset:=site^.next;
      site^.next:=0;
      site^.value:=site^.value + PtrUInt(base);
    end;
end;

procedure FPCRelocateData;
var
  relocH: MemHandle;
  chain: psmallint;
const
  rloc_id = $726c6f63; // 'rloc'
begin
  relocH:=DmGet1Resource(rloc_id, 0);
  if relocH <> nil then
    begin
      chain:=MemHandleLock(relocH);
      RelocateChain(chain^, @data_start);
      Inc(chain);
      RelocateChain(chain^, @_FPC_proc_start);
      Inc(chain);
      MemHandleUnlock(relocH);
      DmReleaseResource(relocH);
    end;
end;

end.
