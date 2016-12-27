{
    This file is part of the Free Pascal run time library.
    Copyright (c) 2016 by Free Pascal development team

    GEMDOS related defines for Atari TOS

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}


{ The API description of this file is based on the information available
  online at: http://toshyp.atari.org }

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

type
    PDTA = ^TDTA;
    TDTA = packed record
        d_reserved: array[0..20] of shortint; {* Reserved for GEMDOS *}
        d_attrib: byte;                       {* File attributes     *}
        d_time: word;                         {* Time                *}
        d_date: word;                         {* Date                *}
        d_length: dword;                      {* File length         *}
        d_fname: array[0..13] of char;        {* Filename            *}
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
      p_env: pchar;           {* Address of the environment string   *}
      p_resrvd1: array[0..79] of char;   {* Reserved                            *}
      p_cmdlin: array[0..127] of char;   {* Command line                        *}
  end;
  TBASEPAGE = TPD; {* alias types... *}
  PBASEPAGE = ^TBASEPAGE;


procedure gemdos_cconws(p: pchar); syscall 1 9;

function gemdos_dsetdrv(drv: smallint): longint; syscall 1 14;

function gemdos_dgetdrv: smallint; syscall 1 25;
procedure gemdos_setdta(buf: PDTA); syscall 1 26;

function gemdos_getdta: PDTA; syscall 1 47;

function gemdos_dfree(buf: PDISKINFO; driveno: smallint): smallint; syscall 1 54;

function gemdos_dcreate(const path: pchar): longint; syscall 1 57;
function gemdos_ddelete(const path: pchar): longint; syscall 1 58;
function gemdos_dsetpath(path: pchar): smallint; syscall 1 59;

function gemdos_fcreate(fname: pchar; attr: smallint): smallint; syscall 1 60;
function gemdos_fopen(fname: pchar; mode: smallint): longint; syscall 1 61;
function gemdos_fclose(handle: smallint): smallint; syscall 1 62;
function gemdos_fread(handle: smallint; count: longint; buf: pointer): longint; syscall 1 63;
function gemdos_fwrite(handle: smallint; count: longint; buf: pointer): longint; syscall 1 64;
function gemdos_fdelete(fname: pchar): smallint; syscall 1 65;
function gemdos_fseek(offset: longint; handle: smallint; seekmode: smallint): longint; syscall 1 66;
function gemdos_fattrib(filename: pchar; wflag: smallint; attrib: smallint): smallint; syscall 1 67;

function gemdos_dgetpath(path: pchar; driveno: smallint): smallint; syscall 1 71;
function gemdos_malloc(number: dword): pointer; syscall 1 72;
function gemdos_free(block: pointer): dword; syscall 1 73;
function gemdos_mshrink(zero: word; block: pointer; newsiz: longint): longint; syscall 1 74;

procedure gemdos_pterm(returncode: smallint); syscall 1 76;

function gemdos_fsfirst(filename: pchar; attr: smallint): longint; syscall 1 78;
function gemdos_fsnext: smallint; syscall 1 79;

function gemdos_frename(zero: word; oldname: pchar; newname: pchar): longint; syscall 1 86;
procedure gemdos_fdatime(timeptr: PDOSTIME; handle: smallint; wflag: smallint); syscall 1 87;
