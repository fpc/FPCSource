{
    Copyright (c) 2016 by Free Pascal development team

    GEMDOS interface unit for Atari TOS

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}

{
    This is used for Pure-Pascal compatibility. For newly written code,
    consider using the gemdos/bios/xbios units instead.
}

{$MODE FPC}
{$MODESWITCH OUT+}
{$LONGSTRINGS OFF} { this unit always uses shortstrings }
{$PACKRECORDS 2}
{$IFNDEF FPC_DOTTEDUNITS}
unit tos;
{$ENDIF FPC_DOTTEDUNITS}

interface

{$IFDEF FPC_DOTTEDUNITS}
uses AtariApi.Gemdos, AtariApi.Xbios, AtariApi.Bios, AtariApi.Metados;
{$ELSE FPC_DOTTEDUNITS}
uses gemdos, xbios, bios, metados;
{$ENDIF FPC_DOTTEDUNITS}

const
    FO_READ     = 0;
    FO_WRITE    = 1;
    FO_RW       = 2;

    FA_RDONLY   = $1;
    FA_HIDDEN   = $2;
    FA_SYSTEM   = $4;
    FA_LABEL    = $8;
    FA_DIREC    = $10;
    FA_ARCH     = $20;
    FA_SYMLINK  = $40;
    FA_ATTRIB   = $17;

type
    DOSTIME = TDOSTIME;
    DOSTIMEPtr = DOSTIME;
    DISKINFO = TDISKINFO;
    DISKINFOPtr = ^DISKINFO;
    LINE = TLINE;
    LINEPtr = ^LINE;
    PD = TBASEPAGE;
    PDPtr = ^PD;
    MD = TMD;
    MDPtr =^MD;
    MPB = TMPB;
    MPBPtr = ^MPB;
    BPB = TBPB;
    BPBPtr = ^BPB;
    XATTR = TXATTR;
    XATTRPtr = ^XATTR;
    MSGTYPE = TMSGTYPE;
    MSGTYPEPtr = ^MSGTYPE;
    SIGACTION = TSIGACTION;
    SIGACTIONPtr = ^SIGACTION;
    MOUSE = TMOUSE;
    MOUSEPtr = ^MOUSE;
    IORECORD = TIORECORD;
    IORECORDPtr = ^IORECORD;
    KEYTAB = TKEYTAB;
    KEYTABPtr = ^KEYTAB;
    KBDVECS = TKBDVECS;
    KBDVECSPtr = ^KBDVECS;
    PBDEF = TPBDEF;
    PBDEFPtr = ^PBDEF;
    DSPBLOCK = TDSPBLOCK;
    DSPBLOCKPtr = ^DSPBLOCK;

    {* same as TDTA in gemdos; except that d_fname is shortstring *}
    DTAPtr = ^DTA;
    DTA = record
        d_reserved :        Array[1..21] of Byte;
        d_attrib :          Byte;
        d_time :            word;
        d_date :            word;
        d_length :          LongInt;
        d_fname :           string[12];
    end;

    LongIntFunc = xbios.TLongIntFunc;

    METAINFO = metados.TMETAINFO;

{ TOS program need this exported }
var
    basepage: PPD; external name '__base';

(* ++++++++++++++++++++++++++++++++++++++++ *)
(*                  BIOS                    *)
(* ++++++++++++++++++++++++++++++++++++++++ *)

procedure Getmpb(out p_mpb: TMPB); syscall 13 0;
function Bconstat(dev: smallint): smallint; syscall 13 1;
function Bconin(dev: smallint): LongInt; syscall 13 2;
procedure Bconout(dev, c: smallint); syscall 13 3;
function Rwabs(rwflag: smallint; buf: Pointer; count, recno, dev: smallint; lrecno: LongInt): LongInt; syscall 13 4;
function Setexc(vecnum: smallint; vec: Pointer): Pointer; syscall 13 5;
function Tickcal: LongInt; syscall 13 6;
function Getbpb(dev: smallint): PBPB; syscall 13 7;
function Bcostat(dev: smallint): LongInt; syscall 13 8;
function Mediach(dev: smallint): LongInt; syscall 13 9;
function Drvmap: LongInt; syscall 13 10;
function Kbshift(mode: smallint): LongInt; syscall 13 11;

(* ++++++++++++++++++++++++++++++++++++++++ *)
(*                  XBIOS                   *)
(* ++++++++++++++++++++++++++++++++++++++++ *)

procedure Initmouse(typ: smallint; var param: TMOUSE; vec: Pointer); syscall 14 0;
function Ssbrk(amount: smallint): Pointer; syscall 14 1;
function physbase: pointer; syscall 14 2;
function logbase: pointer; syscall 14 3;
function getrez: longint; syscall 14 4;
procedure setscreen(laddr: pointer; paddr: pointer; rez: smallint); syscall 14 5;
procedure vsetscreen(laddr: pointer; paddr: pointer; rez: smallint; mode: smallint); syscall 14 5;
procedure setpalette(palette: pointer); syscall 14 6;
function setcolor(colornum: smallint; color: smallint): smallint; syscall 14 7;
function Floprd(buf, filler: Pointer; devno, sectno, trackno, sideno, count: smallint): smallint; syscall 14 8;
function Flopwr(buf, filler: Pointer; devno, sectno, trackno, sideno, count: smallint): smallint; syscall 14 9;
function Flopfmt(buf, filler: Pointer; devno, spt, trackno, sideno, interlv: smallint; magic: LongInt; virgin: Word): smallint; syscall 14 10;
procedure dbmsg(rsrvd: smallint; msg_num: smallint; msg_arg: longint); syscall 14 11;
procedure Midiws(cnt: smallint; ptr: Pointer); syscall 14 12;
procedure Mfpint(interno: smallint; vec: Pointer); syscall 14 13;
function Iorec(devno: smallint): PIORECORD; syscall 14 14;
function Rsconf(speed, flowctl, ucr, rsr, tsr, scr: smallint): LongInt; syscall 14 15;
function Keytbl(unshift, shift, capslock: Pointer): PKEYTAB; syscall 14 16;
function random: longint; syscall 14 17;
procedure Protobt(buf: Pointer; serialno: LongInt; disktype, execflag: smallint); syscall 14 18;
function Flopver(buf, filler: Pointer; devno, sectno, trackno, sideno, count: smallint): smallint; syscall 14 19;
procedure Scrdmp; syscall 14 20;
function Cursconf(mode, operand: smallint): smallint; syscall 14 21;
procedure Settime(datetime: LongInt); syscall 14 22;
function Gettime: LongInt; syscall 14 23;
procedure Bioskeys; syscall 14 24;
procedure Ikbdws(cnt: smallint; ptr: Pointer); syscall 14 25;
procedure Jdisint(intno: smallint); syscall 14 26;
procedure Jenabint(intno: smallint); syscall 14 27;
function Giaccess(data: Byte; regno: smallint): Byte; syscall 14 28;
procedure Offgibit(bitno: smallint); syscall 14 29;
procedure Ongibit(bitno: smallint); syscall 14 30;
procedure Xbtimer(timer, control, data: smallint; vec: Pointer); syscall 14 31;
procedure Dosound(ptr: Pointer); syscall 14 32;
function Setprt(config: smallint): smallint; syscall 14 33;
function Kbdvbase: PKBDVECS; syscall 14 34;
function Kbrate(initial, speed: smallint): smallint; syscall 14 35;
procedure Prtblk(var defptr: TPBDEF); syscall 14 36;
procedure vsync; syscall 14 37;
function Supexec(codeptr: LongIntFunc): LongInt; syscall 14 38;
procedure Puntaes; syscall 14 39;
function Floprate(drive, seekrate: smallint): smallint; syscall 14 41;
function DMAread(sector: LongInt; count: smallint; buffer: Pointer; devno: smallint): LongInt; syscall 14 42;
function DMAwrite(sector: LongInt; count: smallint; buffer: Pointer; devno: smallint): LongInt; syscall 14 43;
function Bconmap(devno: smallint): LongInt; syscall 14 44;
function NVMaccess(op, start, count: smallint; ptr: Pointer): smallint; syscall 14 46;
{* 48-63 reserved for MetaDOS *}
function Blitmode(mode: smallint): smallint; syscall 14 64;
{* 65-79 used by CENTScreen *}
function EsetShift(shftmode: smallint): smallint; syscall 14 80;
function EgetShift: smallint; syscall 14 81;
function EsetBank(bankNum: smallint): smallint; syscall 14 82;
function EsetColor(colorNum, color: smallint): smallint; syscall 14 83;
procedure EsetPalette(colorNum, count: smallint; palettePtr: Pointer); syscall 14 84;
procedure EgetPalette(colorNum, count: smallint; palettePtr: Pointer); syscall 14 85;
function EsetGray(switch: smallint): smallint; syscall 14 86;
function EsetSmear(switch: smallint): smallint; syscall 14 87;
function Vsetmode(modecode: smallint): smallint; syscall 14 88;
function mon_type: smallint; syscall 14 89;
procedure VsetSync(flag: smallint); syscall 14 90;
function VgetSize(mode: smallint): LongInt; syscall 14 91;
procedure VsetRGB(index, count: smallint; xrgbArray: Array of TRGB); syscall 14 93;
procedure VgetRGB(index, count: smallint; out xrgbArray: Array of TRGB); syscall 14 94;
function Validmode(mode: smallint): smallint; syscall 14 95;
procedure Dsp_DoBlock(data_in: Pointer; size_in: LongInt; data_out: Pointer; size_out: LongInt); syscall 14 96;
procedure Dsp_BlkHandShake(data_in: Pointer; size_in: LongInt; data_out: Pointer; size_out: LongInt); syscall 14 97;
procedure Dsp_BlkUnpacked(data_in: Pointer; size_in: LongInt; data_out: Pointer; size_out: LongInt); syscall 14 98;
procedure Dsp_InStream(data_in: Pointer; block_size, num_blocks: LongInt; out blocks_done: LongInt); syscall 14 99;
procedure Dsp_OutStream(data_out: Pointer; block_size, num_blocks: LongInt; out blocks_done: LongInt); syscall 14 100;
procedure Dsp_IOStream(data_in, data_out: Pointer; block_insize, block_outsize, num_blocks: LongInt; out blocks_done: LongInt); syscall 14 101;
procedure Dsp_RemoveInterrupts(mask: smallint); syscall 14 102;
function Dsp_GetWordSize: smallint; syscall 14 103;
function Dsp_Lock: smallint; syscall 14 104;
procedure Dsp_Unlock; syscall 14 105;
procedure Dsp_Available(out xavailable, yavailable: LongInt); syscall 14 106;
function Dsp_Reserve(xreserve, yreserve: LongInt): smallint; syscall 14 107;
function Dsp_LoadProg(const filename: shortstring; ability: smallint; buffer: Pointer): smallint;
procedure Dsp_ExecProg(codeptr: Pointer; codesize: LongInt; ability: smallint); syscall 14 109;
procedure Dsp_ExecBoot(codeptr: Pointer; codesize: LongInt; ability: smallint); syscall 14 110;
function Dsp_LodToBinary(const filename: shortstring; codeptr: Pointer): LongInt;
procedure Dsp_TriggerHC(vector: smallint); syscall 14 112;
function Dsp_RequestUniqueAbility: smallint; syscall 14 113;
function Dsp_GetProgAbility: smallint; syscall 14 114;
procedure Dsp_FlushSubroutines; syscall 14 115;
function Dsp_LoadSubroutine(codeptr: Pointer; codesize: LongInt; ability: smallint): smallint; syscall 14 116;
function Dsp_InqSubrAbility(ability: smallint): smallint; syscall 14 117;
function Dsp_RunSubroutine(handle: smallint): smallint; syscall 14 118;
function Dsp_Hf0(flag: smallint): smallint; syscall 14 119;
function Dsp_Hf1(flag: smallint): smallint; syscall 14 120;
function Dsp_Hf2: smallint; syscall 14 121;
function Dsp_Hf3: smallint; syscall 14 122;
procedure Dsp_BlkWords(data_in: Pointer; size_in: LongInt; data_out: Pointer; size_out: LongInt); syscall 14 123;
procedure Dsp_BlkBytes(data_in: Pointer; size_in: LongInt; data_out: Pointer; size_out: LongInt); syscall 14 124;
function Dsp_HStat: Byte; syscall 14 125;
procedure Dsp_SetVectors(receiver, transmitter: Pointer); syscall 14 126;
procedure Dsp_MultBlocks(numsend, numreceive: LongInt; sendblocks, receiveblocks: PDSPBLOCK); syscall 14 127;
function locksnd: LongInt; syscall 14 128;
function unlocksnd: LongInt; syscall 14 129;
function soundcmd(mode, data: smallint): LongInt; syscall 14 130;
function nsoundcmd(mode, data: smallint; data2: longint): LongInt; syscall 14 130;
function setbuffer(reg: smallint; begaddr, endaddr: Pointer): LongInt; syscall 14 131;
function setmode(mode: smallint): LongInt; syscall 14 132;
function settracks(playtracks, rectracks: smallint): LongInt; syscall 14 133;
function setmontracks(montrack: smallint): LongInt; syscall 14 134;
function setinterrupt(src_inter, cause: smallint): LongInt; syscall 14 135;
function buffoper(mode: smallint): LongInt; syscall 14 136;
function dsptristate(dspxmit, dsprec: smallint): LongInt; syscall 14 137;
function gpio(mode, data: smallint): LongInt; syscall 14 138;
function devconnect(src, dst, srcclk, prescale, protocol: smallint): LongInt; syscall 14 139;
function sndstatus(res: smallint): LongInt; syscall 14 140;
function buffptr(bptr: Pointer): LongInt; syscall 14 141;

procedure VsetMask(ormask, andmask: LongInt; overlay: smallint); syscall 14 150;

procedure Metainit(out buffer: TMETAINFO); syscall 14 48;
function Metaopen(drive: smallint; out buffer: TMETA_DRVINFO): LongInt; syscall 14 49;
function Metaclose(drive: smallint): LongInt; syscall 14 50;
function Metaread(drive: smallint; buffer: Pointer; blockno: LongInt; count: smallint): LongInt; syscall 14 51;
function Metawrite(drive: smallint; buffer: Pointer; blockno: LongInt; count: smallint): LongInt; syscall 14 52;
function Metaseek(drive: smallint; dummy, offset: longint): LongInt; syscall 14 53;
function Metastatus(drive: smallint; buffer: Pointer): LongInt; syscall 14 54;
function Metaioctl(drive: smallint; magic: LongInt; opcode: smallint; buffer: Pointer): LongInt; syscall 14 55;


(* ++++++++++++++++++++++++++++++++++++++++ *)
(*                  GEMDOS                  *)
(* ++++++++++++++++++++++++++++++++++++++++ *)

procedure pterm0; noreturn; syscall 1 0;
function cconin: longint; syscall 1 1;
function cconout(c: smallint): longint; syscall 1 2;
function cauxin: longint; syscall 1 3;
function cauxout(c: smallint): longint; syscall 1 4;
function cprnout(c: smallint): longint; syscall 1 5;
function crawio(c: smallint): longint; syscall 1 6;
function crawin: longint; syscall 1 7;
function cnecin: longint; syscall 1 8;
procedure cconws(const str: shortstring);
function cconrs(buf: PLINE): longint; syscall 1 10;
function cconis: longint; syscall 1 11;

function dsetdrv(drv: smallint): longint; syscall 1 14;

function cconos: smallint; syscall 1 16;
function cprnos: smallint; syscall 1 17;
function cauxis: smallint; syscall 1 18;
function cauxos: smallint; syscall 1 19;
function maddalt(start: pointer; size: longint): longint; syscall 1 20;
function srealloc(len: longint): longint; syscall 1 21;

function dgetdrv: smallint; syscall 1 25;
procedure fsetdta(buf: DTAPtr); syscall 1 26;

function super(stack: pointer): longint; syscall 1 32;

function tgetdate: word; syscall 1 42;
function tsetdate(date: word): word; syscall 1 43;
function tgettime: word; syscall 1 44;
function tsettime(time: word): word; syscall 1 45;

function fgetdta: DTAPtr; syscall 1 47;
function sversion: smallint; syscall 1 48;
procedure ptermres(keepcnt: longint; returncode: smallint); noreturn; syscall 1 49;
function sconfig(mode: smallint; flags: longint): longint; syscall 1 51;

function dfree(out buf: TDISKINFO; driveno: smallint): smallint; syscall 1 54;

function dcreate(const path: shortstring): longint;
function ddelete(const path: shortstring): longint;
function dsetpath(const path: shortstring): smallint;
function fcreate(const fname: shortstring; attr: smallint): smallint;
function fopen(const fname: shortstring; mode: smallint): longint;
function fclose(handle: smallint): smallint; syscall 1 62;
function fread(handle: smallint; count: longint; buf: pointer): longint; syscall 1 63;
function fwrite(handle: smallint; count: longint; buf: pointer): longint; syscall 1 64;
function fdelete(const fname: shortstring): smallint;
function fseek(offset: longint; handle: smallint; seekmode: smallint): longint; syscall 1 66;
function fattrib(const filename: shortstring; wflag: smallint; attrib: smallint): smallint;
function mxalloc(amount: longint; mode: smallint): pointer; syscall 1 68;
function fdup(handle: smallint): smallint; syscall 1 69;
function fforce(stdh: smallint; nonstdh: smallint): smallint; syscall 1 70;
function dgetpath(out path: shortstring; driveno: smallint): smallint;
function malloc(number: dword): pointer; syscall 1 72;
function free(block: pointer): dword; syscall 1 73;
function mfree(block: pointer): dword; syscall 1 73;
function mshrink(zero: word; block: pointer; newsiz: longint): longint; syscall 1 74;
function pexec(mode: word; const name: shortstring; cmdline: shortstring; env: PAnsiChar): longint;
procedure pterm(returncode: smallint); noreturn; syscall 1 76;

function fsfirst(const filename: shortstring; attr: smallint): longint;
function fsnext: smallint;

function frename(const oldname, newname: shortstring): longint;
procedure fdatime(timeptr: PDOSTIME; handle: smallint; wflag: smallint); syscall 1 87;

function Flock(handle, mode: smallint; start, length: LongInt): LongInt; syscall 1 92;

function Syield: smallint; syscall 1 255;
function Fpipe(out usrh: ARRAY of smallint): smallint; syscall 1 256;
function Ffchown(fd, uid, gid: smallint): longint; syscall 1 257;
function Ffchmod(fd: smallint; mode: word): longint; syscall 1 258;
function Fsync(fd: smallint): longint; syscall 1 259;
function Fcntl(f: smallint; arg: LongInt; cmd: smallint): smallint; syscall 1 260;
function Finstat(f: smallint): LongInt; syscall 1 261;
function Foutstat(f: smallint): LongInt; syscall 1 262;
function Fgetchar(f, mode: smallint): LongInt; syscall 1 263;
function Fputchar(f: smallint; c: LongInt; mode: smallint): LongInt; syscall 1 264;
function Pwait: LongInt; syscall 1 265;
function Pnice(delta : smallint): smallint; syscall 1 266;
function Pgetpid: smallint; syscall 1 267;
function Pgetppid: smallint; syscall 1 268;
function Pgetpgrp: smallint; syscall 1 269;
function Psetpgrp(pid, newgrp: smallint): smallint; syscall 1 270;
function Pgetuid: smallint; syscall 1 271;
function Psetuid(id: smallint): smallint; syscall 1 272;
function Pkill(pid, sig: smallint): smallint; syscall 1 273;
function Psignal(sig: smallint; handler: Pointer): Pointer; syscall 1 274;
function Pvfork: smallint; syscall 1 275;
function Pgetgid: smallint; syscall 1 276;
function Psetgid(id : smallint): smallint; syscall 1 277;
function Psigblock(mask: LongInt): LongInt; syscall 1 278;
function Psigsetmask(mask: LongInt): LongInt; syscall 1 279;
function Pusrval(arg: LongInt): LongInt; syscall 1 280;
function Pdomain(newdom: smallint): smallint; syscall 1 281;
procedure Psigreturn; syscall 1 282;
function Pfork: smallint; syscall 1 283;
function Pwait3(flag: smallint; out rusage: ARRAY of LongInt): LongInt; syscall 1 284;
function Fselect(timeout: Word; out rfds, wfds, xfds: LongInt): smallint; syscall 1 285;
function Prusage(out r: ARRAY of LongInt): LongInt; syscall 1 286;
function Psetlimit(lim: smallint; value: LongInt): LongInt; syscall 1 287;
function Talarm(secs: LongInt): LongInt; syscall 1 288;
procedure Pause; syscall 1 289;
function Sysconf(n: smallint): LongInt; syscall 1 290;
function Psigpending: LongInt; syscall 1 291;
function Dpathconf(const name: shortstring; n: smallint): LongInt;
function Pmsg(mode: smallint; mbox: LongInt; var msg: TMSGTYPE): LongInt; syscall 1 293;
function Fmidipipe(pid, inp, outp: smallint): LongInt; syscall 1 294;
function Prenice(pid, delta: smallint): smallint; syscall 1 295;
function Dopendir(const name: shortstring; flag: smallint): LongInt;
function Dreaddir(buflen: smallint; dir: LongInt; out buf: shortstring): LongInt;
function Drewinddir(dir: LongInt): LongInt; syscall 1 298;
function Dclosedir(dir: LongInt): LongInt; syscall 1 299;
function Fxattr(flag: smallint; const name: shortstring; out buf: TXATTR): LongInt;
function Flink(const oldname: shortstring; const newname: shortstring): LongInt;
function Fsymlink(const oldname: shortstring; const newname: shortstring): LongInt;
function Freadlink(size: smallint; out buf: shortstring; const name: shortstring): LongInt;
function Dcntl(cmd: smallint; const name: shortstring; arg: LongInt): LongInt;
function Fchown(const name: shortstring; uid, gid: smallint): LongInt;
function Fchmod(const name: shortstring; mode: smallint): LongInt;
function Pumask(mode: Word): LongInt; syscall 1 307;
function Psemaphore(mode: smallint; id, timeout: LongInt): LongInt; syscall 1 308;
function Dlock(mode, drive: smallint): LongInt; syscall 1 309;
procedure Psigpause(mask: LongInt); syscall 1 310;
function Psigaction(sig: smallint; act, oact: PSIGACTION): LongInt; syscall 1 311;
function Pgeteuid: smallint; syscall 1 312;
function Pgetegid: smallint; syscall 1 313;
function Pwaitpid(pid, flag: smallint; out rusage: ARRAY of LongInt): LongInt; syscall 1 314;
function Dgetcwd(path: PAnsiChar; drv, size: smallint): LongInt; syscall 1 315;
procedure Salert(str: PAnsiChar); syscall 1 316;
function Tmalarm(time: longint): LongInt; syscall 1 317;
{ function Psigintr(vec, sig: smallint): LongInt; syscall 1 318; }
function Suptime(out uptime: longint; out loadaverage: longint): LongInt; syscall 1 319;


(* ++++++++++++++++++++++++++++++++++++++++ *)
(*              IMPLEMENTATION              *)
(* ++++++++++++++++++++++++++++++++++++++++ *)

implementation

function Dsp_LoadProg(const filename: shortstring; ability: smallint; buffer: Pointer): smallint;
var s: array[0..255] of AnsiChar;
begin
  s := filename;
  Dsp_LoadProg := xbios_Dsp_LoadProg(s, ability, buffer);
end;

function Dsp_LodToBinary(const filename: shortstring; codeptr: Pointer): LongInt;
var s: array[0..255] of AnsiChar;
begin
  s := filename;
  Dsp_LodToBinary := xbios_Dsp_LodToBinary(s, codeptr);
end;

procedure cconws(const str: shortstring);
var s: array[0..255] of AnsiChar;
begin
  s := str;
  gemdos_cconws(s);
end;

function pexec(mode: word; const name: shortstring; cmdline: shortstring; env: PAnsiChar): longint;
var s: array[0..255] of AnsiChar;
begin
  s := name;
  pexec := gemdos_pexec(mode, s, @cmdline[0], env);
end;

function fsfirst(const filename: shortstring; attr: smallint): longint;
var s: array[0..255] of AnsiChar;
    dta: DTAPtr;
    temp: string[12];
begin
  s := filename;
  fsfirst := gemdos_fsfirst(s, attr);
  if (fsfirst = 0) then
    begin
      dta := Fgetdta;
      temp := PAnsiChar(@dta^.d_fname[0]);
      dta^.d_fname := temp;
    end;
end;

function fsnext: smallint;
var dta: DTAPtr;
    temp: string[12];
begin
  fsnext := gemdos_fsnext;
  if (fsnext = 0) then
    begin
      dta := Fgetdta;
      temp := PAnsiChar(@dta^.d_fname[0]);
      dta^.d_fname := temp;
    end;
end;

function frename(const oldname, newname: shortstring): longint;
var s1: array[0..255] of AnsiChar;
    s2: array[0..255] of AnsiChar;
begin
  s1 := oldname;
  s2 := newname;
  frename := gemdos_frename(0, s1, s2);
end;

function fopen(const fname: shortstring; mode: smallint): longint;
var s: array[0..255] of AnsiChar;
begin
  s := fname;
  fopen := gemdos_fopen(s, mode);
end;

function fdelete(const fname: shortstring): smallint;
var s: array[0..255] of AnsiChar;
begin
  s := fname;
  fdelete := gemdos_fdelete(s);
end;

function fcreate(const fname: shortstring; attr: smallint): smallint;
var s: array[0..255] of AnsiChar;
begin
  s := fname;
  fcreate := gemdos_fcreate(s, attr);
end;

function fattrib(const filename: shortstring; wflag: smallint; attrib: smallint): smallint;
var s: array[0..255] of AnsiChar;
begin
  s := filename;
  fattrib := gemdos_fattrib(s, wflag, attrib);
end;

function dsetpath(const path: shortstring): smallint;
var s: array[0..255] of AnsiChar;
begin
  s := path;
  dsetpath := gemdos_dsetpath(s);
end;

function dgetpath(out path: shortstring; driveno: smallint): smallint;
var s: array[0..255] of AnsiChar;
begin
  Dgetpath := gemdos_dgetpath(s, driveno);
  if (Dgetpath = 0) then
    path := PAnsiChar(@s[0]);
end;

function ddelete(const path: shortstring): longint;
var s: array[0..255] of AnsiChar;
begin
  s := path;
  Ddelete := gemdos_ddelete(s);
end;

function dcreate(const path: shortstring): longint;
var s: array[0..255] of AnsiChar;
begin
  s := path;
  Dcreate := gemdos_dcreate(s);
end;

function Dpathconf(const name: shortstring; n: smallint): LongInt;
var s: array[0..255] of AnsiChar;
begin
  s := name;
  Dpathconf := gemdos_dpathconf(s, n);
end;

function Dopendir(const name: shortstring; flag: smallint): LongInt;
var s: array[0..255] of AnsiChar;
begin
  s := name;
  Dopendir := gemdos_dopendir(s, flag);
end;

function Dreaddir(buflen: smallint; dir: LongInt; out buf: shortstring): LongInt;
var s: array[0..255] of AnsiChar;
begin
  Dreaddir := gemdos_dreaddir(buflen, dir, s);
  if (dreaddir = 0) then
    buf := PAnsiChar(@s[0]);
end;

function Fxattr(flag: smallint; const name: shortstring; out buf: TXATTR): LongInt;
var s: array[0..255] of AnsiChar;
begin
  s := name;
  Fxattr := gemdos_fxattr(flag, s, buf);
end;

function Flink(const oldname: shortstring; const newname: shortstring): LongInt;
var s1: array[0..255] of AnsiChar;
    s2: array[0..255] of AnsiChar;
begin
  s1 := oldname;
  s2 := newname;
  flink := gemdos_flink(s1, s2);
end;

function Fsymlink(const oldname: shortstring; const newname: shortstring): LongInt;
var s1: array[0..255] of AnsiChar;
    s2: array[0..255] of AnsiChar;
begin
  s1:=oldname;
  s2:=newname;
  fsymlink := gemdos_fsymlink(s1, s2);
end;

function Freadlink(size: smallint; out buf: shortstring; const name: shortstring): LongInt;
var s1: array[0..255] of AnsiChar;
    s2: array[0..255] of AnsiChar;
begin
  s1 := name;
  if (size > 256) then
    size := 256;
  freadlink := gemdos_freadlink(256, s2, s1);
  if (freadlink = 0) then
     buf := PAnsiChar(@s2[0]);
end;

function Dcntl(cmd: smallint; const name: shortstring; arg: LongInt): LongInt;
var s: array[0..255] of AnsiChar;
begin
  s := name;
  Dcntl := gemdos_dcntl(cmd, s, arg);
end;

function Fchown(const name: shortstring; uid, gid: smallint): LongInt;
var s: array[0..255] of AnsiChar;
begin
  s := name;
  Fchown := gemdos_fchown(s, uid, gid);
end;

function Fchmod(const name: shortstring; mode: smallint): LongInt;
var s: array[0..255] of AnsiChar;
begin
  s := name;
  Fchmod := gemdos_fchmod(s, mode);
end;

end.
