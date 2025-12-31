//	File:libapi.h
//	Copyright (C) 1997 by Sony Computer Entertainment Inc.
//			All rights Reserved
//
// $PSLibId: Run-time Library Release 4.6$

// don't change these macros and structures which is referred in controller code

// Prototypes
// prototypes added by suzu 96/03/01 and changed by hakama 96/06/06
unit libapi;
interface
uses libstd;

function SetRCnt(spec: dword; target: word; mode:longint): longint; stdcall; external;
function GetRCnt(spec: dword): longint; stdcall; external;
function ResetRCnt(spec: dword): longint; stdcall; external;
function StartRCnt(spec: dword): longint; stdcall; external;
function StopRCnt(spec: dword): longint; stdcall; external;

function OpenEvent(desc: dword; spec: longint; mode: longint; func: pointer): longint; stdcall; external;
function CloseEvent(event: dword): longint; stdcall; external;
function WaitEvent(event: dword): longint; stdcall; external;
function TestEvent(event: dword): longint; stdcall; external;
function EnableEvent(event: dword): longint; stdcall; external;
function DisableEvent(event: dword): longint; stdcall; external;
procedure DeliverEvent(ev1, ev2: dword); stdcall; external;
procedure UnDeliverEvent(ev1, ev2: dword); stdcall; external;

function OpenTh(func: pointer; sp, gp: dword): longint; stdcall; external;
function CloseTh(thread: dword): longint; stdcall; external;
function ChangeTh(thread: dword): longint; stdcall; external;

function open(devname: pchar; flag: longint): longint; stdcall; external;
function close(fd: longint): longint; stdcall; external;
function lseek(fd: longint; ofs: dword; flag: longint): longint; stdcall; external;
function read(fd: longint; buf: pointer; n: dword): longint; stdcall; external;
function write(fd: longint; buf: pointer; n: dword): longint; stdcall; external;
function ioctl(fd: longint; com, arg: longint): longint; stdcall; external;
function firstfile(name: pchar; dir: PDIRENTRY): PDIRENTRY; stdcall; external;
function nextfile(dir: PDIRENTRY): PDIRENTRY; stdcall; external;
function erase(name: pchar): longint; stdcall; external;


function undelete(name: pchar): longint; stdcall; external;
function format(name: pchar): longint; stdcall; external;
function rename(src, dest: pchar): longint; stdcall; external;
function cd(name: pchar): longint; stdcall; external;

function LoadTest(name: pchar; _exec: PEXEC): longint; stdcall; external;
function Load(name: pchar; _exec: PEXEC): longint; stdcall; external;
//function Exec(_exec: PEXEC; , long, char **): longint; stdcall; external;
function LoadExec(name: pchar; s_addr, s_size: dword): longint; stdcall; external;

function InitPAD(bufA, bufB: pointer; lenA, lenB: dword): longint; stdcall; external;
function StartPAD: longint; stdcall; external;
procedure StopPAD; stdcall; external;
procedure EnablePAD; stdcall; external;
procedure DisablePAD; stdcall; external;

procedure FlushCache; stdcall; external;
procedure ReturnFromException; stdcall; external;
function EnterCriticalSection: longint; stdcall; external;
procedure ExitCriticalSection; stdcall; external;
procedure Exception; stdcall; external;
procedure SwEnterCriticalSection; stdcall; external;
procedure SwExitCriticalSection; stdcall; external;

function SetSp(new_sp: dword): dword; stdcall; external;
function GetSp: dword; stdcall; external;
function GetGp: dword; stdcall; external;
function GetCr: dword; stdcall; external;
function GetSr: dword; stdcall; external;
function GetSysSp: dword; stdcall; external;

function SetConf(ev, tcb, sp: dword): longint; stdcall; external;
procedure GetConf(ev, tcb, sp: pdword); stdcall; external;

function _get_errno: longint; stdcall; external;
function _get_error(fd: longint): longint; stdcall; external;

procedure SystemError(c: char; n: dword); stdcall; external;
procedure SetMem(n: dword); stdcall; external;

function Krom2RawAdd(sjiscode: word): longint; stdcall; external;
function Krom2RawAdd2(sjiscode: word): longint; stdcall; external;

procedure _96_init; stdcall; external;
procedure _96_remove; stdcall; external;
procedure _boot; stdcall; external;

procedure ChangeClearPAD(val: longint); stdcall; external;

// prototypes added by shino 96/05/22
procedure InitCARD(val: longint); stdcall; external;
function StartCARD: longint; stdcall; external;
function StopCARD: longint; stdcall; external;
procedure _bu_init; stdcall; external;
function _card_info(chan: longint): longint; stdcall; external;
function _card_clear(chan: longint): longint; stdcall; external;
function _card_load(chan: longint): longint; stdcall; external;
function _card_auto(val: longint): longint; stdcall; external;
procedure _new_card; stdcall; external;
function _card_status(drv: longint): longint; stdcall; external;
function _card_wait(drv: longint): longint; stdcall; external;
function _card_chan: dword; stdcall; external;
function _card_write(chan, block: longint; buf: pointer): longint; stdcall; external;
function _card_read(chan, block: longint; buf: pointer): longint; stdcall; external;
function _card_format(chan: longint): longint; stdcall; external;	// added by iwano 98/03/24



implementation
begin
end.