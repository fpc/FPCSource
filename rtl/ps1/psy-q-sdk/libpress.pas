// libpress.h
unit libpress;
interface
type
// DecDCTvlc Table
    DECDCTTAB = array [0..34815] of word;

    DECDCTENV = packed record
	               iq_y : array [0..63] of byte;	   // IQ (Y): zig-zag order
	               iq_c : array [0..63] of byte;	   // IQ (Cb,Cr): zig-zag order
	               dct : array [0..63] of smallint;    // IDCT coef (reserved)
    end;
    PDECDCTENV = ^DECDCTENV;

    ENCSPUENV = packed record
                    src : psmallint;		  // 16-bit strait PCM
                    dest : psmallint;		  // PlayStation original waveform data
                    work : psmallint;		  // scratch pad or NULL
                    size : longint;		      // size (unit: byte) of source data
                    loop_start : longint;	  // loop start point (unit: byte) of source data
                    loop : byte;		      // whether loop or not
                    byte_swap : byte;		  // source data is 16-bit big endian (1) / little endian (0)
                    proceed : byte;		      // proceeding ? whole (0) / start (1) / cont. (2) / end (4)
                    quality : byte;		      // quality ? middle (0) / high (1)
    end;
    PENCSPUENV = ^ENCSPUENV;

    DecDCTinCallbackFunc = procedure;

const
    ENCSPU_ENCODE_ERROR    = -1;
    ENCSPU_ENCODE_WHOLE    = 0;
    ENCSPU_ENCODE_START    = 1 shl 0;
    ENCSPU_ENCODE_CONTINUE = 1 shl 1;
    ENCSPU_ENCODE_END      = 1 shl 2;

    ENCSPU_ENCODE_LOOP     = 1;
    ENCSPU_ENCODE_NO_LOOP  = 0;

    ENCSPU_ENCODE_ENDIAN_LITTLE = 0;
    ENCSPU_ENCODE_ENDIAN_BIG    = 1;

    ENCSPU_ENCODE_MIDDLE_QULITY = 0;
    ENCSPU_ENCODE_HIGH_QULITY   = 1;


procedure DecDCTReset(mode: longint); external;
function DecDCTGetEnv(env: PDECDCTENV): PDECDCTENV; external;
function DecDCTPutEnv(env: PDECDCTENV): PDECDCTENV; external;
function DecDCTBufSize(bs: pdword): longint; external;
function DecDCTvlc(bs: pdword; buf: pdword): longint; external;
function DecDCTvlc2(bs: pdword; buf: pdword; table: DECDCTTAB): longint; external;
function DecDCTvlcSize(size: longint): longint; external;
function DecDCTvlcSize2(size: longint): longint; external;
procedure DecDCTvlcBuild(table: pword); external;
procedure DecDCTin(buf: pdword; mode: longint); external;
procedure DecDCTout(buf: pdword; size: longint); external;
function DecDCTinSync(mode: longint): longint; external;
function DecDCToutSync(mode: longint): longint; external;

function DecDCTinCallback(func: DecDCTinCallbackFunc): longint; external;
function DecDCToutCallback(func: DecDCTinCallbackFunc): longint; external;

function EncSPU (env: PENCSPUENV): longint; external;
function EncSPU2(env: PENCSPUENV): longint; external;

implementation

begin
end.