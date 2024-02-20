{
    Copyright (c) 2016 by Free Pascal development team

    GEMDOS interface unit for Atari TOS

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}

{$PACKRECORDS 2}
{$MODESWITCH OUT+}
{$IFNDEF FPC_DOTTEDUNITS}
unit gemdos;
{$ENDIF FPC_DOTTEDUNITS}

interface

{ The API description of this file is based on the information available
  online at: https://freemint.github.io/tos.hyp/en/index.html }

const
    E_OK        = 0;       // OK. No error has arisen
    EINVFN      = -32;     // Unknown function number
    EFILNF      = -33;     // File not found
    EPTHNF      = -34;     // Directory (folder) not found
    ENHNDL      = -35;     // No more handles available
    EACCDN      = -36;     // Access denied
    EIHNDL      = -37;     // Invalid file handle
    ENSMEM      = -39;     // Insufficient memory
    EIMBA       = -40;     // Invalid memory block address
    EDRIVE      = -46;     // Invalid drive specification
    ECWD        = -47;     // Current directory cannot be deleted
    ENSAME      = -48;     // Files on different logical drives
    ENMFIL      = -49;     // No more files can be opened
    ELOCKED     = -58;     // Segment of a file is protected (network)
    ENSLOCK     = -59;     // Invalid lock removal request
    ERANGE      = -64;     // File pointer in invalid segment (see also FreeMiNT message -88)
    EINTRN      = -65;     // Internal error of GEMDOS
    EPLFMT      = -66;     // Invalid program load format
    EGSBF       = -67;     // Allocated memory block could not be enlarged
    EBREAK      = -68;     // Program termination by Control-C
    EXCPT       = -69;     // 68000 exception (bombs)
    EPTHOV      = -70;     // Path overflow
    ELOOP       = -80;     // Endless loop with symbolic links
    EPIPE       = -81;     // Write to broken pipe.

// as used by fseek
const
    SEEK_FROM_START   = 0;
    SEEK_FROM_CURRENT = 1;
    SEEK_FROM_END     = 2;

// as used by fcreate and fattrib
const
    ATTRIB_WRITE_PROT   = ( 1 shl 0 );
    ATTRIB_HIDDEN       = ( 1 shl 1 );
    ATTRIB_SYSTEM       = ( 1 shl 2 );
    ATTRIB_VOLUME_LABEL = ( 1 shl 3 );
    ATTRIB_DIRECTORY    = ( 1 shl 4 );
    ATTRIB_ARCHIVE      = ( 1 shl 5 );

// as used by fopen
const
    OPEN_READ_ONLY      = ( 1 shl 0 );
    OPEN_WRITE_ONLY     = ( 1 shl 1 );
    OPEN_READ_WRITE     = ( 1 shl 2 );

// as used by fattrib
const
    FLAG_GET            = 0;
    FLAG_SET            = 1;

// as used by mxalloc
const
    MXALLOC_ST          = 0;
    MXALLOC_ALT         = 1;
    MXALLOC_PREFER_ST   = 2;
    MXALLOC_PREFER_ALT  = 3;

    MXALLOC_PROT_DEFAULT     = (0 shl 4);
    MXALLOC_PROT_PRIVATE     = (1 shl 4);
    MXALLOC_PROT_GLOBAL      = (2 shl 4);
    MXALLOC_PROT_SUPER_ONLY  = (3 shl 4);
    MXALLOC_PROT_WORLD_READ  = (4 shl 4);

    MXALLOC_NO_FREE          = (1 shl 14);

// as used by pexec
const
    PE_LOADGO           = 0;          {* load & go *}
    PE_LOAD             = 3;          {* just load *}
    PE_GO               = 4;          {* just go *}
    PE_CBASEPAGE        = 5;          {* just create basepage *}
    PE_GO_FREE          = 6;          {* just go, then free *}
    PE_CBASEPAGE_FLAGS  = 7;          {* create basepage defining the program flags *}
    {* MiNT/MagiC *}
    PE_ASYNC_LOADGO     = 100;        {* load and asynchronously go *}
    PE_INIT             = 101;        {* Inherits path- and file-handles (MagiC) *}
    PE_TERM             = 102;        {* The process will be deleted (MagiC) *}
    PE_ASYNC_GO         = 104;        {* asynchronously go *}
    PE_ASYNC_GO_FREE    = 106;        {* asynchronously go and free *}
    PE_XBASE            = 107;        {* As Mode 7, but in place of a command line the process name will be passed. (MagiC) *}
    PE_EXACC            = 108;        {* Used internally by MagiC for starting an ACC *}
    PE_OVERLAY          = 200;        {* load and overlay   *}
    PE_OVERLAY_GO       = 204;        {* overlay and go *}
    PE_OVERLAY_GO_FREE  = 206;        {* overlay, go and free *}
    PE_PTRACE           = $8000;      {* activate tracing *}

// Sconfig - Modes
    SC_GETCONF   = 0;                 {* KAOS 1.2 *}
    SC_SETCONF   = 1;                 {* KAOS 1.2 *}
    SC_DOSVARS   = 2;                 {* KAOS 1.4 *}
    SC_MOWNER    = 3;                 {* MagiC 3.00 *}
    SC_WBACK     = 4;                 {* MagiC 4.01 *}
    SC_INTMAVAIL = 5;                 {* MagiC 4.01 *}
    SC_INTGARBC  = 6;                 {* MagiC 4.01 *}

// Sconfig Bits
    SCB_PTHCK   = $001;               {* KAOS 1.2 *}
    SCB_DSKCH   = $002;               {* KAOS 1.2 *}
    SCB_BREAK   = $004;               {* KAOS 1.2 *}
    SCB_NCTLC   = $008;               {* KAOS 1.2 *}
    SCB_NFAST   = $010;               {* KAOS 1.2 *}
    SCB_CMPTB   = $020;               {* KAOS 1.4 *}
    SCB_NSMRT   = $040;               {* KAOS 1.4 *}
    SCB_NGRSH   = $080;               {* KAOS 1.4 *}
    SCB_NHALT   = $100;               {* KAOS 1.4 *}
    SCB_RESVD   = $200;               {* KAOS 1.4 *}
    SCB_PULLM   = $400;               {* KAOS 1.4 *}
    SCB_FLPAR   = $800;               {* MagiC 3.0 *}

type
    PDTA = ^TDTA;
    TDTA = packed record
        d_reserved: array[0..20] of shortint; {* Reserved for GEMDOS *}
        d_attrib: byte;                       {* File attributes     *}
        d_time: word;                         {* Time                *}
        d_date: word;                         {* Date                *}
        d_length: dword;                      {* File length         *}
        d_fname: array[0..13] of AnsiChar;        {* Filename            *}
    end;

type
    PDISKINFO = ^TDISKINFO;
    TDISKINFO = record
        b_free: dword;        {* Number of free clusters  *}
        b_total: dword;       {* Total number of clusters *}
        b_secsiz: dword;      {* Bytes per sector         *}
        b_clsiz: dword;       {* Sector per cluster       *}
    end;

type
    PDOSTIME = ^TDOSTIME;
    TDOSTIME = record
        time: word;           {* Time like Tgettime *}
        date: word;           {* Date like Tgetdate *}
    end;

type
  PPD = ^TPD;
  TPD = record
      p_lowtpa: pointer;      {* Start address of the TPA            *}
      p_hitpa: pointer;       {* First byte after the end of the TPA *}
      p_tbase: pointer;       {* Start address of the program code   *}
      p_tlen: longint;        {* Length of the program code          *}
      p_dbase: pointer;       {* Start address of the DATA segment   *}
      p_dlen: longint;        {* Length of the DATA section          *}
      p_bbase: pointer;       {* Start address of the BSS segment    *}
      p_blen: longint;        {* Length of the BSS section           *}
      p_dta: PDTA;            {* Pointer to the default DTA          *}
                              {* Warning: Points first to the        *}
                              {* command line !                      *}
      p_parent: PPD;          {* Pointer to the basepage of the      *}
                              {* calling processes                   *}
      p_resrvd0: longint;     {* Reserved                            *}
      p_env: PAnsiChar;           {* Address of the environment string   *}
      p_resrvd1: array[0..79] of AnsiChar;   {* Reserved                            *}
      p_cmdlin: array[0..127] of AnsiChar;   {* Command line                        *}
  end;
  TBASEPAGE = TPD; {* alias types... *}
  PBASEPAGE = ^TBASEPAGE;

type
  PLINE = ^TLINE;
  TLINE = record
      maxlen: byte;        {* Maximum line length *}
      actuallen: byte;     {* Current line length *}
      buffer: array[0..254] of byte;   {* Line buffer         *}
  end;


procedure gemdos_pterm0; noreturn; syscall 1 0;
function gemdos_cconin: longint; syscall 1 1;
function gemdos_cconout(c: smallint): longint; syscall 1 2;
function gemdos_cauxin: longint; syscall 1 3;
function gemdos_cauxout(c: smallint): longint; syscall 1 4;
function gemdos_cprnout(c: smallint): longint; syscall 1 5;
function gemdos_crawio(c: smallint): longint; syscall 1 6;
function gemdos_crawin: longint; syscall 1 7;
function gemdos_cnecin: longint; syscall 1 8;
procedure gemdos_cconws(p: PAnsiChar); syscall 1 9;
function gemdos_cconrs(buf: PLINE): longint; syscall 1 10;
function gemdos_cconis: longint; syscall 1 11;

function gemdos_dsetdrv(drv: smallint): longint; syscall 1 14;

function gemdos_cconos: smallint; syscall 1 16;
function gemdos_cprnos: smallint; syscall 1 17;
function gemdos_cauxis: smallint; syscall 1 18;
function gemdos_cauxos: smallint; syscall 1 19;
function gemdos_maddalt(start: pointer; size: longint): longint; syscall 1 20;
function gemdos_srealloc(len: longint): longint; syscall 1 21;

function gemdos_dgetdrv: smallint; syscall 1 25;
procedure gemdos_fsetdta(buf: PDTA); syscall 1 26;

function gemdos_super(stack: pointer): longint; syscall 1 32;

function gemdos_tgetdate: word; syscall 1 42;
function gemdos_tsetdate(date: word): word; syscall 1 43;
function gemdos_tgettime: word; syscall 1 44;
function gemdos_tsettime(time: word): word; syscall 1 45;

function gemdos_fgetdta: PDTA; syscall 1 47;
function gemdos_sversion: smallint; syscall 1 48;
procedure gemdos_ptermres(keepcnt: longint; returncode: smallint); noreturn; syscall 1 49;
function gemdos_sconfig(mode: smallint; flags: longint): longint; syscall 1 51;

function gemdos_dfree(var buf: TDISKINFO; driveno: smallint): smallint; syscall 1 54;

function gemdos_dcreate(const path: PAnsiChar): longint; syscall 1 57;
function gemdos_ddelete(const path: PAnsiChar): longint; syscall 1 58;
function gemdos_dsetpath(const path: PAnsiChar): smallint; syscall 1 59;
function gemdos_fcreate(const fname: PAnsiChar; attr: smallint): smallint; syscall 1 60;
function gemdos_fopen(const fname: PAnsiChar; mode: smallint): longint; syscall 1 61;
function gemdos_fclose(handle: smallint): smallint; syscall 1 62;
function gemdos_fread(handle: smallint; count: longint; buf: pointer): longint; syscall 1 63;
function gemdos_fwrite(handle: smallint; count: longint; buf: pointer): longint; syscall 1 64;
function gemdos_fdelete(const fname: PAnsiChar): smallint; syscall 1 65;
function gemdos_fseek(offset: longint; handle: smallint; seekmode: smallint): longint; syscall 1 66;
function gemdos_fattrib(const filename: PAnsiChar; wflag: smallint; attrib: smallint): smallint; syscall 1 67;
function gemdos_mxalloc(amount: longint; mode: smallint): pointer; syscall 1 68;
function gemdos_fdup(handle: smallint): smallint; syscall 1 69;
function gemdos_fforce(stdh: smallint; nonstdh: smallint): smallint; syscall 1 70;
function gemdos_dgetpath(path: PAnsiChar; driveno: smallint): smallint; syscall 1 71;
function gemdos_malloc(number: dword): pointer; syscall 1 72;
function gemdos_mfree(block: pointer): dword; syscall 1 73;
function gemdos_mshrink(zero: word; block: pointer; newsiz: longint): longint; syscall 1 74;
function gemdos_pexec(mode: word; name: PAnsiChar; cmdline: PAnsiChar; env: PAnsiChar): longint; syscall 1 75;
procedure gemdos_pterm(returncode: smallint); noreturn; syscall 1 76;

function gemdos_fsfirst(const filename: PAnsiChar; attr: smallint): longint; syscall 1 78;
function gemdos_fsnext: smallint; syscall 1 79;

function gemdos_frename(zero: word; const oldname: PAnsiChar; const newname: PAnsiChar): longint; syscall 1 86;
procedure gemdos_fdatime(timeptr: PDOSTIME; handle: smallint; wflag: smallint); syscall 1 87;

function gemdos_Flock(handle, mode: smallint; start, length: LongInt): LongInt; syscall 1 92;

{ MiNT extensions }

type
    PMSGTYPE = ^TMSGTYPE;
    TMSGTYPE = record
        msg1:              LongInt;
        msg2:              LongInt;
        pid:               SmallInt;
    end;

    PXATTR = ^TXATTR;
    TXATTR = record
        mode:              Word;
        index:             LongInt;
        dev:               Word;
        rdev:              Word;
        nlink:             Word;
        uid:               Word;
        gid:               Word;
        size:              LongInt;
        blksize:           LongInt;
        nblocks:           LongInt;
        mtime:             Word;
        mdate:             Word;
        atime:             Word;
        adate:             Word;
        ctime:             Word;
        cdate:             Word;
        attr:              Word;
        reserved2:         Word;
        reserved3:         Array[0..1] of LongInt;
    end;

    PSIGACTION = ^TSIGACTION;
    TSIGACTION = record
        sa_handler:        Pointer;
        sa_mask:           LongInt;
        sa_flags:          Integer;
    end;

function gemdos_Syield: smallint; syscall 1 255;
function gemdos_Fpipe(var usrh: ARRAY of smallint): smallint; syscall 1 256;
function gemdos_Ffchown(fd, uid, gid: smallint): longint; syscall 1 257;
function gemdos_Ffchmod(fd: smallint; mode: word): longint; syscall 1 258;
function gemdos_Fsync(fd: smallint): longint; syscall 1 259;
function gemdos_Fcntl(f: smallint; arg: LongInt; cmd: smallint): smallint; syscall 1 260;
function gemdos_Finstat(f: smallint): LongInt; syscall 1 261;
function gemdos_Foutstat(f: smallint): LongInt; syscall 1 262;
function gemdos_Fgetchar(f, mode: smallint): LongInt; syscall 1 263;
function gemdos_Fputchar(f: smallint; c: LongInt; mode: smallint): LongInt; syscall 1 264;
function gemdos_Pwait: LongInt; syscall 1 265;
function gemdos_Pnice(delta : smallint): smallint; syscall 1 266;
function gemdos_Pgetpid: smallint; syscall 1 267;
function gemdos_Pgetppid: smallint; syscall 1 268;
function gemdos_Pgetpgrp: smallint; syscall 1 269;
function gemdos_Psetpgrp(pid, newgrp: smallint): smallint; syscall 1 270;
function gemdos_Pgetuid: smallint; syscall 1 271;
function gemdos_Psetuid(id: smallint): smallint; syscall 1 272;
function gemdos_Pkill(pid, sig: smallint): smallint; syscall 1 273;
function gemdos_Psignal(sig: smallint; handler: Pointer): Pointer; syscall 1 274;
function gemdos_Pvfork: smallint; syscall 1 275;
function gemdos_Pgetgid: smallint; syscall 1 276;
function gemdos_Psetgid(id : smallint): smallint; syscall 1 277;
function gemdos_Psigblock(mask: LongInt): LongInt; syscall 1 278;
function gemdos_Psigsetmask(mask: LongInt): LongInt; syscall 1 279;
function gemdos_Pusrval(arg: LongInt): LongInt; syscall 1 280;
function gemdos_Pdomain(newdom: smallint): smallint; syscall 1 281;
procedure gemdos_Psigreturn; syscall 1 282;
function gemdos_Pfork: smallint; syscall 1 283;
function gemdos_Pwait3(flag: smallint; var rusage: ARRAY of LongInt): LongInt; syscall 1 284;
function gemdos_Fselect(timeout: Word; var rfds, wfds, xfds: LongInt): smallint; syscall 1 285;
function gemdos_Prusage(var r: ARRAY of LongInt): LongInt; syscall 1 286;
function gemdos_Psetlimit(lim: smallint; value: LongInt): LongInt; syscall 1 287;
function gemdos_Talarm(secs: LongInt): LongInt; syscall 1 288;
procedure gemdos_Pause; syscall 1 289;
function gemdos_Sysconf(n: smallint): LongInt; syscall 1 290;
function gemdos_Psigpending: LongInt; syscall 1 291;
function gemdos_Dpathconf(const name: PAnsiChar; n: smallint): LongInt; syscall 1 292;
function gemdos_Pmsg(mode: smallint; mbox: LongInt; var msg: TMSGTYPE): LongInt; syscall 1 293;
function gemdos_Fmidipipe(pid, inp, outp: smallint): LongInt; syscall 1 294;
function gemdos_Prenice(pid, delta: smallint): smallint; syscall 1 295;
function gemdos_Dopendir(const name: PAnsiChar; flag: smallint): LongInt; syscall 1 296;
function gemdos_Dreaddir(buflen: smallint; dir: LongInt; buf: PAnsiChar): LongInt; syscall 1 297;
function gemdos_Drewinddir(dir: LongInt): LongInt; syscall 1 298;
function gemdos_Dclosedir(dir: LongInt): LongInt; syscall 1 299;
function gemdos_Fxattr(flag: smallint; const name: PAnsiChar; var buf: TXATTR): LongInt; syscall 1 300;
function gemdos_Flink(const oldname: PAnsiChar; const newname: PAnsiChar): LongInt; syscall 1 301;
function gemdos_Fsymlink(const oldname: PAnsiChar; const newname: PAnsiChar): LongInt; syscall 1 302;
function gemdos_Freadlink(size: smallint; buf: PAnsiChar; const name: PAnsiChar): LongInt; syscall 1 303;
function gemdos_Dcntl(cmd: smallint; const name: PAnsiChar; arg: LongInt): LongInt; syscall 1 304;
function gemdos_Fchown(const name: PAnsiChar; uid, gid: smallint): LongInt; syscall 1 305;
function gemdos_Fchmod(const name: PAnsiChar; mode: smallint): LongInt; syscall 1 306;
function gemdos_Pumask(mode: Word): LongInt; syscall 1 307;
function gemdos_Psemaphore(mode: smallint; id, timeout: LongInt): LongInt; syscall 1 308;
function gemdos_Dlock(mode, drive: smallint): LongInt; syscall 1 309;
procedure gemdos_Psigpause(mask: LongInt); syscall 1 310;
function gemdos_Psigaction(sig: smallint; act, oact: PSIGACTION): LongInt; syscall 1 311;
function gemdos_Pgeteuid: smallint; syscall 1 312;
function gemdos_Pgetegid: smallint; syscall 1 313;
function gemdos_Pwaitpid(pid, flag: smallint; var rusage: ARRAY of LongInt): LongInt; syscall 1 314;
function gemdos_Dgetcwd(path: PAnsiChar; drv, size: smallint): LongInt; syscall 1 315;
procedure gemdos_Salert(str: PAnsiChar); syscall 1 316;
function gemdos_Tmalarm(time: longint): LongInt; syscall 1 317;
{ function gemdos_Psigintr(vec, sig: smallint): LongInt; syscall 1 318; }
function gemdos_Suptime(var uptime: longint; var loadaverage: longint): LongInt; syscall 1 319;

implementation

end.
