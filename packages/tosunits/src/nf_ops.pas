{
    This file is part of the Free Pascal run time library.
    Copyright (c) 2022 Thorsten Otto

    NatFeats interface unit for Atari TOS

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}

{$X+}
{$I-}
{$Q-}
{$R-}
{$S-}
{$B-}

{$IFNDEF FPC_DOTTEDUNITS}
unit NF_OPS;
{$ENDIF FPC_DOTTEDUNITS}

interface

const
    NF_ID_NAME      : PAnsiChar = 'NF_NAME';
    NF_ID_VERSION   : PAnsiChar = 'NF_VERSION';
    NF_ID_STDERR    : PAnsiChar = 'NF_STDERR';
    NF_ID_SHUTDOWN  : PAnsiChar = 'NF_SHUTDOWN';
    NF_ID_EXIT      : PAnsiChar = 'NF_EXIT';
    NF_ID_DEBUG     : PAnsiChar = 'DEBUGPRINTF';
    NF_ID_ETHERNET  : PAnsiChar = 'ETHERNET';
    NF_ID_HOSTFS    : PAnsiChar = 'HOSTFS';
    NF_ID_AUDIO     : PAnsiChar = 'AUDIO';
    NF_ID_BOOTSTRAP : PAnsiChar = 'BOOTSTRAP';
    NF_ID_CDROM     : PAnsiChar = 'CDROM';
    NF_ID_CLIPBRD   : PAnsiChar = 'CLIPBRD';
    NF_ID_JPEG      : PAnsiChar = 'JPEG';
    NF_ID_OSMESA    : PAnsiChar = 'OSMESA';
    NF_ID_PCI       : PAnsiChar = 'PCI';
    NF_ID_FVDI      : PAnsiChar = 'fVDI';
    NF_ID_USBHOST   : PAnsiChar = 'USBHOST';
    NF_ID_XHDI      : PAnsiChar = 'XHDI';
    NF_ID_SCSI      : PAnsiChar = 'NF_SCSIDRV';
    NF_ID_HOSTEXEC  : PAnsiChar = 'HOSTEXEC';
    NF_ID_CONFIG    : PAnsiChar = 'NF_CONFIG';

(*
 * return the NF id to use for feature_name,
 *  or zero when not available.
 *)
function nf_get_id(feature_name: PAnsiChar): longint;

(*
 * return the version of the NatFeat implementation,
 *  or zero when not available.
 *)
function nf_version: longint;

(*
 * return the name of the NatFeat implementor,
 *  or NULL when not available.
 *)
procedure nf_get_name(buf: PAnsiChar; bufsize: longint);

(*
 * return the full name of the NatFeat implementor,
 *  or NULL when not available.
 *)
procedure nf_get_fullname(buf: PAnsiChar; bufsize: longint);

(*
 * Write a string to the host's terminal.
 * returns TRUE when available, FALSE otherwise.
 *)
function nf_debug(const s: string): boolean;

(*
 * Shutdown the emulator.
 * May only be called from Supervisor.
 *)
function nf_shutdown(mode: integer): longint;

(*
 * Shutdown the emulator.
 * May be called from user mode.
 *)
function nf_exit(exitcode: integer): longint;


implementation

{$IFDEF FPC_DOTTEDUNITS}
uses
    AtariApi.Xbios;
{$ELSE FPC_DOTTEDUNITS}
uses
    xbios;
{$ENDIF FPC_DOTTEDUNITS}

const
    NATFEAT_ID = $7300;
    NATFEAT_CALL = $7301;

var
    nf_available: boolean;
    nf_inited: boolean;
    nf_stderr: longint;

type
   Tnf_id = function(id: PAnsiChar): longint; cdecl;
   Tnf_call = function(id: longint): longint; cdecl; varargs;

var cnf_call: Tnf_call;

var ps: array[0..255] of AnsiChar;

const nf_id_opcodes: array[0..1] of word = (NATFEAT_ID, $4e75);
      nf_call_opcodes: array[0..1] of word = (NATFEAT_CALL, $4e75);

function nf_id(id: PAnsiChar): longint;
var cnf_id: Tnf_id;
begin
  cnf_id := Tnf_id(@nf_id_opcodes);
  nf_id := cnf_id(id);
end;


const nf_version_str: array[0..11] of AnsiChar = 'NF_VERSION';

function nf_detect: longint; assembler; nostackframe;
asm
{$IFDEF CPUCFV4E}
 (*
  * on ColdFire, the NATFEAT_ID opcode is actually
  * "mvs.b d0,d1".
  * But since there is no emulator that emulates a ColdFire,
  * this feature is not available.
  *)
  moveq #0,d0
{$ELSE}
  pea    nf_version_str
  moveq  #0,d0      (* assume no NatFeats available *)
  move.l d0,-(sp)
  lea    @nf_illegal,a1
  move.l $0010,a0   (* illegal instruction vector *)
  move.l a1,$0010
  move.l sp,a1      (* save the ssp *)

  nop               (* flush pipelines (for 68040+) *)

  dc.w   NATFEAT_ID (* Jump to NATFEAT_ID *)
  tst.l  d0
  beq.s  @nf_illegal
  moveq  #1,d0      (* NatFeats detected *)
  move.l d0,(sp)

@nf_illegal:
  move.l a1,sp
  move.l a0,$0010
  nop               (* flush pipelines (for 68040+) *)
  move.l (sp)+,d0
  addq.l #4,sp      (* pop nf_version argument *)
{$ENDIF}
end;

function nf_init: boolean;
var ret: longint;
begin
  if not nf_inited then
    begin
      ret := xbios_supexec(@nf_detect);
      nf_available := ret <> 0;
      nf_inited := true;
      cnf_call := Tnf_call(@nf_call_opcodes);
    end;
  nf_init := nf_available;
end;


function nf_get_id(feature_name: PAnsiChar): longint;
begin
  nf_get_id := 0;
  if nf_init then
    nf_get_id := nf_id(feature_name);
end;

function nf_version: longint;
var id: longint;
begin
  nf_version := 0;
  id := nf_get_id(NF_ID_VERSION);
  if id <> 0 then
    nf_version := cnf_call(id);
end;

procedure nf_get_name(buf: PAnsiChar; bufsize: longint);
var id: longint;
begin
  id := nf_get_id(NF_ID_NAME);
  if id <> 0 then
    cnf_call(id or 0, buf, bufsize)
  else
    buf^ := #0;
end;

procedure nf_get_fullname(buf: PAnsiChar; bufsize: longint);
var id: longint;
begin
  id := nf_get_id(NF_ID_NAME);
  if id <> 0 then
    cnf_call(id or 1, buf, bufsize)
  else
    buf^ := #0;
end;

function nf_debug(const s: string): boolean;
begin
  ps := s;
  nf_debug := false;
  if nf_stderr = 0 then
    nf_stderr := nf_get_id(NF_ID_STDERR);
  if nf_stderr <> 0 then
    begin
      cnf_call(nf_stderr, Addr(ps[0]));
      nf_debug := true;
    end;
end;

function nf_shutdown(mode: integer): longint;
var id: longint;
begin
  nf_shutdown := 0;
  id := nf_get_id(NF_ID_SHUTDOWN);
  if id <> 0 then
    nf_shutdown := cnf_call(id or mode);
end;

function nf_exit(exitcode: integer): longint;
var id: longint;
begin
  nf_exit := 0;
  id := nf_get_id(NF_ID_EXIT);
  if id <> 0 then
    nf_exit := cnf_call(id or 0, longint(exitcode));
end;

begin
end.
