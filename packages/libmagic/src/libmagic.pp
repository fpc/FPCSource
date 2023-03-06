{
    This file is part of the Free Pascal packages
    Copyright (C) 2019 Silvio Clecio (silvioprog)

    Pascal binding for libmagic(3)

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
}

{$IFNDEF FPC_DOTTEDUNITS}
unit libmagic;
{$ENDIF FPC_DOTTEDUNITS}

{$IFDEF FPC}
 {$MODE OBJFPC}
 {$H+}
{$ENDIF}

interface

type
  Pcchar = PAnsiChar;
  cint = LongInt;
  csize_t = NativeUInt;
  Pcsize_t = PNativeUInt;
  Pcvoid = Pointer;
  PPcvoid = PPointer;

const
  MAGIC_LIB_NAME = {$IFDEF MSWINDOWS}'libmagic-1'{$ELSE}'magic'{$ENDIF};

const
  MAGIC_NONE = $0000000; // No flags
  MAGIC_DEBUG = $0000001; // Turn on debugging
  MAGIC_SYMLINK = $0000002; // Follow symlinks
  MAGIC_COMPRESS = $0000004; // Check inside compressed files
  MAGIC_DEVICES = $0000008; // Look at the contents of devices
  MAGIC_MIME_TYPE = $0000010; // Return the MIME type
  MAGIC_CONTINUE = $0000020; // Return all matches
  MAGIC_CHECK_ = $0000040; // Print warnings to stderr
  MAGIC_PRESERVE_ATIME = $0000080; // Restore access time on exit
  MAGIC_RAW = $0000100; // Don't convert unprintable chars
  MAGIC_ERROR_ = $0000200; // Handle ENOENT etc as real errors
  MAGIC_MIME_ENCODING = $0000400; // Return the MIME encoding
  MAGIC_MIME = MAGIC_MIME_TYPE or MAGIC_MIME_ENCODING;
  MAGIC_APPLE = $0000800; // Return the Apple creator/type
  MAGIC_EXTENSION  = $1000000; // Return a /-separated list of extensions
  MAGIC_COMPRESS_TRANSP = $2000000; // Check inside compressed files but not report compression
  MAGIC_NODESC = MAGIC_EXTENSION or MAGIC_MIME or MAGIC_APPLE;

const
  MAGIC_NO_CHECK_COMPRESS = $0001000; // Don't check for compressed files
  MAGIC_NO_CHECK_TAR = $0002000; // Don't check for tar files
  MAGIC_NO_CHECK_SOFT = $0004000; // Don't check magic entries
  MAGIC_NO_CHECK_APPTYPE = $0008000; // Don't check application type
  MAGIC_NO_CHECK_ELF = $0010000; // Don't check for elf details
  MAGIC_NO_CHECK_TEXT = $0020000; // Don't check for text files
  MAGIC_NO_CHECK_CDF = $0040000; // Don't check for cdf files
  MAGIC_NO_CHECK_TOKENS = $0100000; // Don't check tokens
  MAGIC_NO_CHECK_ENCODING = $0200000; // Don't check text encodings

const
  // No built-in tests; only consult the magic file
  MAGIC_NO_CHECK_BUILTIN =
    MAGIC_NO_CHECK_COMPRESS or
    MAGIC_NO_CHECK_TAR or
    //MAGIC_NO_CHECK_SOFT or
    MAGIC_NO_CHECK_APPTYPE or
    MAGIC_NO_CHECK_ELF or
    MAGIC_NO_CHECK_TEXT or
    MAGIC_NO_CHECK_CDF or
    MAGIC_NO_CHECK_TOKENS or
    MAGIC_NO_CHECK_ENCODING or
    0;

const
  MAGIC_SNPRINTB = #177#020+
    'b'#0'debug'#0+
    'b'#1'symlink'#0+
    'b'#2'compress'#0+
    'b'#3'devices'#0+
    'b'#4'mime_type'#0+
    'b'#5'continue'#0+
    'b'#6'check'#0+
    'b'#7'preserve_atime'#0+
    'b'#10'raw'#0+
    'b'#11'error'#0+
    'b'#12'mime_encoding'#0+
    'b'#13'apple'#0+
    'b'#14'no_check_compress'#0+
    'b'#15'no_check_tar'#0+
    'b'#16'no_check_soft'#0+
    'b'#17'no_check_sapptype'#0+
    'b'#20'no_check_elf'#0+
    'b'#21'no_check_text'#0+
    'b'#22'no_check_cdf'#0+
    'b'#23'no_check_reserved0'#0+
    'b'#24'no_check_tokens'#0+
    'b'#25'no_check_encoding'#0+
    'b'#26'no_check_reserved1'#0+
    'b'#27'no_check_reserved2'#0+
    'b'#30'extension'#0+
    'b'#31'transp_compression'#0;

const
  // Defined for backwards compatibility (renamed)
  MAGIC_NO_CHECK_ASCII = MAGIC_NO_CHECK_TEXT;

const
  // Defined for backwards compatibility; do nothing
  MAGIC_NO_CHECK_FORTRAN = $000000; // Don't check ascii/fortran
  MAGIC_NO_CHECK_TROFF = $000000; // Don't check ascii/troff

const
  MAGIC_VERSION_ = 532; // This implementation

type
  magic_t = ^magic_set;
  magic_set = record
  end;

function magic_open(flags: cint): magic_t; cdecl; external MAGIC_LIB_NAME name 'magic_open';
procedure magic_close(cookie: magic_t); cdecl; external MAGIC_LIB_NAME name 'magic_close';

function magic_getpath(const magicfile: Pcchar; action: cint): Pcchar; cdecl; external MAGIC_LIB_NAME name 'magic_getpath';
function magic_file(cookie: magic_t; const filename: Pcchar): Pcchar; cdecl; external MAGIC_LIB_NAME name 'magic_file';
function magic_descriptor(cookie: magic_t; fd: cint): Pcchar; cdecl; external MAGIC_LIB_NAME name 'magic_descriptor';
function magic_buffer(cookie: magic_t; const buffer: Pcvoid; length: csize_t): Pcchar; cdecl; external MAGIC_LIB_NAME name 'magic_buffer';

function magic_error(cookie: magic_t): Pcchar; cdecl; external MAGIC_LIB_NAME name 'magic_error';
function magic_getflags(cookie: magic_t): cint; cdecl; external MAGIC_LIB_NAME name 'magic_getflags';
function magic_setflags(cookie: magic_t; flags: cint): cint; cdecl; external MAGIC_LIB_NAME name 'magic_setflags';

function magic_version: cint; cdecl; external MAGIC_LIB_NAME name 'magic_version';
function magic_load(cookie: magic_t; const filename: Pcchar): cint; cdecl; external MAGIC_LIB_NAME name 'magic_load';
function magic_load_buffers(cookie: magic_t; buffers: PPcvoid; sizes: Pcsize_t; nbuffers: csize_t): cint; cdecl; external MAGIC_LIB_NAME name 'magic_load_buffers';

function magic_compile(cookie: magic_t; const filename: Pcchar): cint; cdecl; external MAGIC_LIB_NAME name 'magic_compile';
function magic_check(cookie: magic_t; const filename: Pcchar): cint; cdecl; external MAGIC_LIB_NAME name 'magic_check';
function magic_list(cookie: magic_t; const filename: Pcchar): cint; cdecl; external MAGIC_LIB_NAME name 'magic_list';
function magic_errno(cookie: magic_t): cint; cdecl; external MAGIC_LIB_NAME name 'magic_errno';

const
  MAGIC_PARAM_INDIR_MAX = 0;
  MAGIC_PARAM_NAME_MAX = 1;
  MAGIC_PARAM_ELF_PHNUM_MAX = 2;
  MAGIC_PARAM_ELF_SHNUM_MAX = 3;
  MAGIC_PARAM_ELF_NOTES_MAX = 4;
  MAGIC_PARAM_REGEX_MAX = 5;
  MAGIC_PARAM_BYTES_MAX = 6;

function magic_setparam(cookie: magic_t; param: cint; const value: Pcvoid): cint; cdecl; external MAGIC_LIB_NAME name 'magic_setparam';
function magic_getparam(cookie: magic_t; param: cint; value: Pcvoid): cint; cdecl; external MAGIC_LIB_NAME name 'magic_getparam';

implementation

end.
