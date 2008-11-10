{$ifndef NO_SMART_LINK}
{$smartlink on}
{$endif}
unit zlib;

interface

{ Needed for array of const }
{$mode objfpc}
{ for linux for linking with libc }
{$ifdef unix}
  {$linklib c}
{$endif}
{$PACKRECORDS 4}

uses
  ctypes;

const
  ZLIB_VERSION = '1.1.3';

{$ifdef netware}  {zlib.nlm comes with netware6}
  libz='zlib';
{$else}
  {$ifdef windows}
    libz='zlib1';
  {$else windows}
    libz='z';
  {$endif windows}
{$endif}

{$ifndef windows}
{$linklib libz}
{$endif windows}

type
  { Compatible with paszlib }
  Uint    = Longint;
  Ulong   = Longint;
  Ulongf  = Longint;
  Pulongf = ^Ulongf;
  z_off_t = longint;
  pbyte   = ^byte;
  pbytef  = ^byte;

  TAllocfunc = function (opaque:pointer; items:uInt; size:uInt):pointer;cdecl;
  TFreeFunc = procedure (opaque:pointer; address:pointer);cdecl;

  TInternalState = record
    end;
  PInternalState = ^TInternalstate;

  TZStream = record
    next_in : pbytef;
    avail_in : uInt;
    total_in : uLong;
    next_out : pbytef;
    avail_out : uInt;
    total_out : uLong;
    msg : pbytef;
    state : PInternalState;
    zalloc : TAllocFunc;
    zfree : TFreeFunc;
    opaque : pointer;
    data_type : longint;
    adler : uLong;
    reserved : uLong;
  end;
  TZStreamRec = TZStream;
  PZstream = ^TZStream;
  gzFile = pointer;


const
  Z_NO_FLUSH = 0;

  Z_PARTIAL_FLUSH = 1;
  Z_SYNC_FLUSH = 2;
  Z_FULL_FLUSH = 3;
  Z_FINISH = 4;

  Z_OK = 0;
  Z_STREAM_END = 1;
  Z_NEED_DICT = 2;
  Z_ERRNO = -(1);
  Z_STREAM_ERROR = -(2);
  Z_DATA_ERROR = -(3);
  Z_MEM_ERROR = -(4);
  Z_BUF_ERROR = -(5);
  Z_VERSION_ERROR = -(6);

  Z_NO_COMPRESSION = 0;
  Z_BEST_SPEED = 1;
  Z_BEST_COMPRESSION = 9;
  Z_DEFAULT_COMPRESSION = -(1);

  Z_FILTERED = 1;
  Z_HUFFMAN_ONLY = 2;
  Z_DEFAULT_STRATEGY = 0;

  Z_BINARY = 0;
  Z_ASCII = 1;
  Z_UNKNOWN = 2;

  Z_DEFLATED = 8;

  Z_NULL = 0;

function zlibVersionpchar:pchar;cdecl;external libz name 'zlibVersion';
function zlibVersion:string;
function deflate(var strm:TZStream; flush:longint):longint;cdecl;external libz name 'deflate';
function deflateEnd(var strm:TZStream):longint;cdecl;external libz name 'deflateEnd';
function inflate(var strm:TZStream; flush:longint):longint;cdecl;external libz name 'inflate';
function inflateEnd(var strm:TZStream):longint;cdecl;external libz name 'inflateEnd';
function deflateSetDictionary(var strm:TZStream;dictionary : pbytef; dictLength:uInt):longint;cdecl;external libz name 'deflateSetDictionary';
function deflateCopy(var dest,source:TZstream):longint;cdecl;external libz name 'deflateCopy';
function deflateReset(var strm:TZStream):longint;cdecl;external libz name 'deflateReset';
function deflateParams(var strm:TZStream; level:longint; strategy:longint):longint;cdecl;external libz name 'deflateParams';
function inflateSetDictionary(var strm:TZStream;dictionary : pbytef; dictLength:uInt):longint;cdecl;external libz name 'inflateSetDictionary';
function inflateSync(var strm:TZStream):longint;cdecl;external libz name 'inflateSync';
function inflateReset(var strm:TZStream):longint;cdecl;external libz name 'inflateReset';
function compress(dest:pbytef;destLen:puLongf; source : pbytef; sourceLen:uLong):cint;cdecl;external libz name 'compress';
function compress2(dest:pbytef;destLen:puLongf; source : pbytef; sourceLen:uLong; level:cint):cint;cdecl;external libz name 'compress2';
function uncompress(dest:pbytef;destLen:puLongf; source : pbytef; sourceLen:uLong):cint;cdecl;external libz name 'uncompress';
function gzopen(path:pchar; mode:pchar):gzFile;cdecl;external libz name 'gzopen';
function gzdopen(fd:longint; mode:pchar):gzFile;cdecl;external libz name 'gzdopen';
function gzsetparams(thefile:gzFile; level:longint; strategy:longint):longint;cdecl;external libz name 'gzsetparams';
function gzread(thefile:gzFile; buf:pointer; len:cardinal):longint;cdecl;external libz name 'gzread';
function gzwrite(thefile:gzFile; buf:pointer; len:cardinal):longint;cdecl;external libz name 'gzwrite';
function gzprintf(thefile:gzFile; format:pbytef; args:array of const):longint;cdecl;external libz name 'gzprintf';
function gzputs(thefile:gzFile; s:pbytef):longint;cdecl;external libz name 'gzputs';
function gzgets(thefile:gzFile; buf:pbytef; len:longint):pbytef;cdecl;external libz name 'gzgets';
function gzputc(thefile:gzFile; c:char):char;cdecl;external libz name 'gzputc';
function gzgetc(thefile:gzFile):char;cdecl;external libz name 'gzgetc';
function gzflush(thefile:gzFile; flush:longint):longint;cdecl;external libz name 'gzflush';
function gzseek(thefile:gzFile; offset:z_off_t; whence:longint):z_off_t;cdecl;external libz name 'gzseek';
function gzrewind(thefile:gzFile):longint;cdecl;external libz name 'gzrewind';
function gztell(thefile:gzFile):z_off_t;cdecl;external libz name 'gztell';
function gzeof(thefile:gzFile):longbool;cdecl;external libz name 'gzeof';
function gzclose(thefile:gzFile):longint;cdecl;external libz name 'gzclose';
function gzerror(thefile:gzFile; var errnum:longint):pbytef;cdecl;external libz name 'gzerror';
function adler32(adler:uLong;buf : pbytef; len:uInt):uLong;cdecl;external libz name 'adler32';
function crc32(crc:uLong;buf : pbytef; len:uInt):uLong;cdecl;external libz name 'crc32';
function deflateInit_(var strm:TZStream; level:longint; version:pchar; stream_size:longint):longint;cdecl;external libz name 'deflateInit_';
function inflateInit_(var strm:TZStream; version:pchar; stream_size:longint):longint;cdecl;external libz name 'inflateInit_';
function deflateInit(var strm:TZStream;level : longint) : longint;
function inflateInit(var strm:TZStream) : longint;
function deflateInit2_(var strm:TZStream; level:longint; method:longint; windowBits:longint; memLevel:longint;strategy:longint; version:pchar; stream_size:longint):longint;cdecl;external libz name 'deflateInit2_';
function inflateInit2_(var strm:TZStream; windowBits:longint; version:pchar; stream_size:longint):longint;cdecl;external libz name 'inflateInit2_';
function deflateInit2(var strm:TZStream;level,method,windowBits,memLevel,strategy : longint) : longint;
function inflateInit2(var strm:TZStream;windowBits : longint) : longint;
function zErrorpchar(err:longint):pchar;cdecl;external libz name 'zError';
function zError(err:longint):string;
function inflateSyncPoint(z:PZstream):longint;cdecl;external libz name 'inflateSyncPoint';
function get_crc_table:pointer;cdecl;external libz name 'get_crc_table';

function zlibAllocMem(AppData: Pointer; Items, Size: Integer): Pointer; cdecl;
procedure zlibFreeMem(AppData, Block: Pointer);  cdecl;

implementation

uses
  strings;

function zlibversion : string;
  begin
     zlibversion:=strpas(zlibversionpchar);
  end;

function deflateInit(var strm:TZStream;level : longint) : longint;
  begin
     deflateInit:=deflateInit_(strm,level,ZLIB_VERSION,sizeof(TZStream));
  end;

function inflateInit(var strm:TZStream) : longint;
  begin
     inflateInit:=inflateInit_(strm,ZLIB_VERSION,sizeof(TZStream));
  end;

function deflateInit2(var strm:TZStream;level,method,windowBits,memLevel,strategy : longint) : longint;
  begin
     deflateInit2:=deflateInit2_(strm,level,method,windowBits,memLevel,strategy,ZLIB_VERSION,sizeof(TZStream));
  end;

function inflateInit2(var strm:TZStream;windowBits : longint) : longint;
  begin
     inflateInit2:=inflateInit2_(strm,windowBits,ZLIB_VERSION,sizeof(TZStream));
  end;

function zError(err:longint):string;
  begin
     zerror:=Strpas(zErrorpchar(err));
  end;

function zlibAllocMem(AppData: Pointer; Items, Size: Integer): Pointer; cdecl;

  begin
    Result := AllocMem(Items * Size);
  end;

procedure zlibFreeMem(AppData, Block: Pointer);  cdecl;

  begin
    FreeMem(Block);
  end;


end.
