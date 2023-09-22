{$IFNDEF FPC_DOTTEDUNITS}
unit PasZLib;
{$ENDIF FPC_DOTTEDUNITS}

{$inline on}

interface

{$IFDEF FPC_DOTTEDUNITS}
uses
  System.ZLib.Zbase;
{$ELSE FPC_DOTTEDUNITS}
uses
  zbase;
{$ENDIF FPC_DOTTEDUNITS}

const
  ZLIB_VERSION = '1.2';

type
  { Compatibility types }
  z_off_t = longint;

  TInternalState = record
    end;
  PInternalState = ^TInternalstate;

  TZStream = z_stream;
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

  Z_NULL = nil;

function zlibVersion:string;inline;
function deflate(var strm:TZstream; flush:longint):longint;inline;
function deflateEnd(var strm:TZstream):longint;inline;
function inflate(var strm:TZstream; flush:longint):longint;inline;
function inflateEnd(var strm:TZstream):longint;inline;
function deflateSetDictionary(var strm:TZstream;dictionary : PAnsiChar; dictLength:cardinal):longint;inline;
function deflateCopy(var dest,source:TZstream):longint;inline;
function deflateReset(var strm:TZstream):longint;inline;
function deflateParams(var strm:TZstream; level:longint; strategy:longint):longint;inline;
function inflateSetDictionary(var strm:TZStream;dictionary : PAnsiChar; dictLength:cardinal):longint;inline;
function inflateSync(var strm:TZStream):longint;inline;
function inflateReset(var strm:TZStream):longint;inline;
function compress(dest:PAnsiChar;var destLen:cardinal; source : PAnsiChar; sourceLen:cardinal):longint;
function compress2(dest:PAnsiChar;var destLen:cardinal; source : PAnsiChar; sourceLen:cardinal; level:longint):longint;
function uncompress(dest:PAnsiChar;var destLen:cardinal; source : PAnsiChar; sourceLen:cardinal):longint;
function gzopen(path:PAnsiChar; mode:PAnsiChar):gzFile;inline;
function gzsetparams(Thefile:gzFile; level:longint; strategy:longint):longint;inline;
function gzread(thefile:gzFile; buf : pointer; len:cardinal):longint;inline;
function gzwrite(thefile:gzFile; buf: pointer; len:cardinal):longint;inline;
function gzputs(thefile:gzFile; s:PAnsiChar):longint;inline;
function gzgets(thefile:gzFile; buf:PAnsiChar; len:longint):PAnsiChar;inline;
function gzputc(thefile:gzFile; c:AnsiChar):longint;inline;
function gzgetc(thefile:gzFile):AnsiChar;inline;
function gzflush(thefile:gzFile; flush:longint):longint;inline;
function gzseek(thefile:gzFile; offset:z_off_t; whence:longint):z_off_t;inline;
function gzrewind(thefile:gzFile):longint;inline;
function gztell(thefile:gzFile):z_off_t;inline;
function gzeof(thefile:gzFile):longbool;inline;
function gzclose(thefile:gzFile):longint;inline;
function gzerror(thefile:gzFile; var errnum:smallint):string;inline;
function adler32(theadler:cardinal;buf : PAnsiChar; len:cardinal):cardinal;inline;
function crc32(thecrc:cardinal;buf : PAnsiChar; len:cardinal):cardinal;inline;
function deflateInit_(var strm:TZStream; level:longint; version:PAnsiChar; stream_size:longint):longint;inline;
function inflateInit_(var strm:TZStream; version:PAnsiChar; stream_size:longint):longint;inline;
function deflateInit2_(var strm:TZStream; level:longint; method:longint; windowBits:longint; memLevel:longint;strategy:longint; version:PAnsiChar; stream_size:longint):longint;inline;
function inflateInit2_(var strm:TZStream; windowBits:longint; version:PAnsiChar; stream_size:longint):longint;inline;
function deflateInit(var strm:TZStream;level : longint) : longint;inline;
function inflateInit(var strm:TZStream) : longint;inline;
function deflateInit2(var strm:TZStream;level,method,windowBits,memLevel,strategy : longint) : longint;inline;
function inflateInit2(var strm:TZStream; windowBits : longint) : longint;inline;
function zError(err:longint):string;inline;
function inflateSyncPoint(z:PZstream):longint;inline;
function get_crc_table:pointer;inline;

implementation

{$IFDEF FPC_DOTTEDUNITS}
uses
  System.ZLib.Zdeflate,System.ZLib.Zinflate,System.ZLib.Zcompres,System.ZLib.Zuncompr,System.ZLib.Gzio,System.ZLib.Adler,System.Hash.Crc;
{$ELSE FPC_DOTTEDUNITS}
uses
  zdeflate,zinflate,zcompres,zuncompr,gzio,adler,crc;
{$ENDIF FPC_DOTTEDUNITS}

function zlibVersion:string;inline;
begin
  zlibversion:={$IFDEF FPC_DOTTEDUNITS}System.ZLib.{$ENDIF}zbase.zlibversion;
end;

function deflate(var strm:TZstream; flush:longint):longint;inline;
begin
  deflate:={$IFDEF FPC_DOTTEDUNITS}System.ZLib.{$ENDIF}zdeflate.deflate(strm,flush);
end;

function deflateEnd(var strm:TZstream):longint;inline;
begin
  deflateEnd:={$IFDEF FPC_DOTTEDUNITS}System.ZLib.{$ENDIF}zdeflate.deflateEnd(strm);
end;

function inflate(var strm:TZstream; flush:longint):longint;inline;
begin
  inflate:={$IFDEF FPC_DOTTEDUNITS}System.ZLib.{$ENDIF}zinflate.inflate(strm,flush);
end;

function inflateEnd(var strm:TZstream):longint;inline;
begin
  inflateEnd:={$IFDEF FPC_DOTTEDUNITS}System.ZLib.{$ENDIF}zinflate.inflateEnd(strm);
end;

function deflateSetDictionary(var strm:TZstream;dictionary : PAnsiChar; dictLength:cardinal):longint;inline;
begin
  deflateSetDictionary:={$IFDEF FPC_DOTTEDUNITS}System.ZLib.{$ENDIF}zdeflate.deflateSetDictionary(strm,Pbyte(dictionary),dictlength);
end;

function deflateCopy(var dest,source:TZstream):longint;inline;
begin
  deflateCopy:={$IFDEF FPC_DOTTEDUNITS}System.ZLib.{$ENDIF}zdeflate.deflateCopy(@dest,@source);
end;

function deflateReset(var strm:TZstream):longint;inline;
begin
  deflateReset:={$IFDEF FPC_DOTTEDUNITS}System.ZLib.{$ENDIF}zdeflate.deflateReset(strm);
end;

function deflateParams(var strm:TZstream; level:longint; strategy:longint):longint;inline;
begin
  deflateParams:={$IFDEF FPC_DOTTEDUNITS}System.ZLib.{$ENDIF}zdeflate.deflateParams(strm,level,strategy);
end;

function inflateSetDictionary(var strm:TZStream;dictionary : PAnsiChar; dictLength:cardinal):longint;inline;
begin
  inflateSetDictionary:={$IFDEF FPC_DOTTEDUNITS}System.ZLib.{$ENDIF}zinflate.inflateSetDictionary(strm,Pbyte(dictionary),dictlength);
end;

function inflateSync(var strm:TZStream):longint;inline;
begin
  inflateSync:={$IFDEF FPC_DOTTEDUNITS}System.ZLib.{$ENDIF}zinflate.inflateSync(strm);
end;

function inflateReset(var strm:TZStream):longint;inline;
begin
  inflateReset:={$IFDEF FPC_DOTTEDUNITS}System.ZLib.{$ENDIF}zinflate.inflateReset(strm);
end;

function compress(dest:PAnsiChar;var destLen:cardinal; source : PAnsiChar; sourceLen:cardinal):longint;

type Pbytearray=^Tbytearray;
     Tbytearray=array[0..0] of byte;

begin
  compress:={$IFDEF FPC_DOTTEDUNITS}System.ZLib.{$ENDIF}zcompres.compress(Pbyte(dest),destlen,Pbytearray(source)^,sourcelen);
end;

function compress2(dest:PAnsiChar;var destLen:cardinal; source : PAnsiChar; sourceLen:cardinal; level:longint):longint;

type Pbytearray=^Tbytearray;
     Tbytearray=array[0..0] of byte;

begin
  compress2:={$IFDEF FPC_DOTTEDUNITS}System.ZLib.{$ENDIF}zcompres.compress2(Pbyte(dest),destlen,Pbytearray(source)^,sourcelen,level);
end;

function uncompress(dest:PAnsiChar;var destLen:cardinal; source : PAnsiChar; sourceLen:cardinal):longint;

type Pbytearray=^Tbytearray;
     Tbytearray=array[0..0] of byte;

begin
  uncompress:={$IFDEF FPC_DOTTEDUNITS}System.ZLib.{$ENDIF}zuncompr.uncompress(Pbyte(dest),destlen,Pbytearray(source)^,sourcelen);
end;

function gzopen(path:PAnsiChar; mode:PAnsiChar):gzFile;inline;
begin
  gzopen:={$IFDEF FPC_DOTTEDUNITS}System.ZLib.{$ENDIF}gzio.gzopen(path,mode);
end;

function gzsetparams(Thefile:gzFile; level:longint; strategy:longint):longint;inline;
begin
  gzsetparams:={$IFDEF FPC_DOTTEDUNITS}System.ZLib.{$ENDIF}gzio.gzsetparams(thefile,level,strategy);
end;

function gzread(thefile:gzFile; buf : pointer; len:cardinal):longint;inline;
begin
  gzread:={$IFDEF FPC_DOTTEDUNITS}System.ZLib.{$ENDIF}gzio.gzread(thefile,buf,len);
end;

function gzwrite(thefile:gzFile; buf: pointer; len:cardinal):longint;inline;
begin
  gzwrite:={$IFDEF FPC_DOTTEDUNITS}System.ZLib.{$ENDIF}gzio.gzwrite(thefile,buf,len);
end;

function gzputs(thefile:gzFile; s:PAnsiChar):longint;inline;
begin
  gzputs:={$IFDEF FPC_DOTTEDUNITS}System.ZLib.{$ENDIF}gzio.gzputs(thefile,s);
end;

function gzgets(thefile:gzFile; buf:PAnsiChar; len:longint):PAnsiChar;inline;
begin
  gzgets:={$IFDEF FPC_DOTTEDUNITS}System.ZLib.{$ENDIF}gzio.gzgets(thefile,buf,len);
end;

function gzputc(thefile:gzFile; c:AnsiChar):longint;inline;
begin
  gzputc:={$IFDEF FPC_DOTTEDUNITS}System.ZLib.{$ENDIF}gzio.gzputc(thefile,c);
end;

function gzgetc(thefile:gzFile):AnsiChar;inline;
begin
  gzgetc:=chr({$IFDEF FPC_DOTTEDUNITS}System.ZLib.{$ENDIF}gzio.gzgetc(thefile));
end;

function gzflush(thefile:gzFile; flush:longint):longint;inline;
begin
  gzflush:={$IFDEF FPC_DOTTEDUNITS}System.ZLib.{$ENDIF}gzio.gzflush(thefile,flush);
end;

function gzseek(thefile:gzFile; offset:z_off_t; whence:longint):z_off_t;inline;
begin
  gzseek:={$IFDEF FPC_DOTTEDUNITS}System.ZLib.{$ENDIF}gzio.gzseek(thefile,offset,whence);
end;

function gzrewind(thefile:gzFile):longint;inline;
begin
  gzrewind:={$IFDEF FPC_DOTTEDUNITS}System.ZLib.{$ENDIF}gzio.gzrewind(thefile);
end;

function gztell(thefile:gzFile):z_off_t;inline;
begin
  gztell:={$IFDEF FPC_DOTTEDUNITS}System.ZLib.{$ENDIF}gzio.gztell(thefile);
end;

function gzeof(thefile:gzFile):longbool;inline;
begin
  gzeof:={$IFDEF FPC_DOTTEDUNITS}System.ZLib.{$ENDIF}gzio.gzeof(thefile);
end;

function gzclose(thefile:gzFile):longint;inline;
begin
  gzclose:={$IFDEF FPC_DOTTEDUNITS}System.ZLib.{$ENDIF}gzio.gzclose(thefile);
end;

function gzerror(thefile:gzFile; var errnum:smallint):string;inline;
begin
  gzerror:={$IFDEF FPC_DOTTEDUNITS}System.ZLib.{$ENDIF}gzio.gzerror(thefile,errnum);
end;

function adler32(theadler:cardinal;buf : PAnsiChar; len:cardinal):cardinal;inline;
begin
  adler32:={$IFDEF FPC_DOTTEDUNITS}System.ZLib.{$ENDIF}adler.adler32(theadler,Pbyte(buf),len);
end;

function crc32(thecrc:cardinal;buf : PAnsiChar; len:cardinal):cardinal;inline;
begin
  crc32:={$IFDEF FPC_DOTTEDUNITS}System.Hash.{$ENDIF}crc.crc32(thecrc,Pbyte(buf),len);
end;

function deflateInit_(var strm:TZStream; level:longint; version:PAnsiChar; stream_size:longint):longint;inline;
begin
  deflateInit_:={$IFDEF FPC_DOTTEDUNITS}System.ZLib.{$ENDIF}zdeflate.deflateInit_(@strm,level,version,stream_size);
end;

function inflateInit_(var strm:TZStream; version:PAnsiChar; stream_size:longint):longint;inline;
begin
  inflateInit_:={$IFDEF FPC_DOTTEDUNITS}System.ZLib.{$ENDIF}zinflate.inflateInit_(@strm,version,stream_size);
end;

function deflateInit2_(var strm:TZStream; level:longint; method:longint; windowBits:longint; memLevel:longint;strategy:longint; version:PAnsiChar; stream_size:longint):longint;inline;
begin
  deflateInit2_:={$IFDEF FPC_DOTTEDUNITS}System.ZLib.{$ENDIF}zdeflate.deflateInit2_(strm,level,method,windowBits,memlevel,strategy,version,stream_size);
end;

function inflateInit2_(var strm:TZStream; windowBits:longint; version:PAnsiChar; stream_size:longint):longint;inline;
begin
  inflateInit2_:={$IFDEF FPC_DOTTEDUNITS}System.ZLib.{$ENDIF}zinflate.inflateInit2_(strm,windowBits,version,stream_size);
end;

function deflateInit(var strm:TZStream;level : longint) : longint;inline;
begin
  deflateInit:={$IFDEF FPC_DOTTEDUNITS}System.ZLib.{$ENDIF}zdeflate.deflateInit(strm,level);
end;

function inflateInit(var strm:TZStream) : longint;inline;
begin
  inflateInit:={$IFDEF FPC_DOTTEDUNITS}System.ZLib.{$ENDIF}zinflate.inflateInit(strm);
end;

function deflateInit2(var strm:TZStream;level,method,windowBits,memLevel,strategy : longint) : longint;inline;
begin
  deflateInit2:={$IFDEF FPC_DOTTEDUNITS}System.ZLib.{$ENDIF}zdeflate.deflateInit2(strm,level,method,windowbits,memlevel,strategy);
end;

function inflateInit2(var strm:TZStream; windowBits : longint) : longint;inline;
begin
  inflateInit2:={$IFDEF FPC_DOTTEDUNITS}System.ZLib.{$ENDIF}zinflate.inflateInit2_(strm,windowBits,ZLIB_VERSION,sizeof(TZStream));
end;

function zError(err:longint):string;inline;
begin
  zerror:={$IFDEF FPC_DOTTEDUNITS}System.ZLib.{$ENDIF}zbase.zerror(err);
end;

function inflateSyncPoint(z:PZstream):longint;inline;
begin
  inflateSyncPoint:={$IFDEF FPC_DOTTEDUNITS}System.ZLib.{$ENDIF}zinflate.inflateSyncPoint(z^);
end;

function get_crc_table:pointer;inline;
begin
  get_crc_table:={$IFDEF FPC_DOTTEDUNITS}System.Hash.{$ENDIF}crc.get_crc_table;
end;

end.
