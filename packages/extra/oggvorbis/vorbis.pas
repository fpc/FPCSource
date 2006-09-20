(********************************************************************
 *                                                                  *
 * THIS FILE IS PART OF THE OggVorbis SOFTWARE CODEC SOURCE CODE.   *
 * USE, DISTRIBUTION AND REPRODUCTION OF THIS LIBRARY SOURCE IS     *
 * GOVERNED BY A BSD-STYLE SOURCE LICENSE INCLUDED WITH THIS SOURCE *
 * IN 'COPYING'. PLEASE READ THESE TERMS BEFORE DISTRIBUTING.       *
 *                                                                  *
 * THE OggVorbis SOURCE CODE IS (C) COPYRIGHT 1994-2001             *
 * by the XIPHOPHORUS Company http://www.xiph.org/                  *
 *                                                                  *
 ********************************************************************)

unit vorbis;

interface

{$MINENUMSIZE 4}

uses
  LibXCPPTypes,
  libogg;

{.$DEFINE DYNLINK}

{$IFNDEF FPC}
  {$DEFINE DYNLINK}
{$ENDIF}

{$IFNDEF DYNLINK}
  {$LINKLIB vorbis}
  {$LINKLIB vorbisfile}
//  {$LINKLIB libvorbisenc.a}
{$ENDIF}

(***********************************************************************)
(* Header : codec.h                                                    *)
(***********************************************************************)

type
  pcmfloat = ^pfloat;

  pvorbis_info = ^vorbis_info;
  vorbis_info = record
    version         : int;
    channels        : int;
    rate            : long;

  { The below bitrate declarations are *hints*.
     Combinations of the three values carry the following implications:

     all three set to the same value:
       implies a fixed rate bitstream
     only nominal set:
       implies a VBR stream that averages the nominal bitrate.  No hard
       upper/lower limit
     upper and or lower set:
       implies a VBR bitstream that obeys the bitrate limits. nominal
       may also be set to give a nominal rate.
     none set:
       the coder does not care to speculate.
  }

    bitrate_upper   : long;
    bitrate_nominal : long;
    bitrate_lower   : long;
    bitrate_window  : long;
    codec_setup     : pointer;
  end;

{ vorbis_dsp_state buffers the current vorbis audio analysis/synthesis state.  The DSP state belongs to a specific logical bitstream }

  pvorbis_dsp_state = ^vorbis_dsp_state;
  vorbis_dsp_state = record
    analysisp       : int;
    vi              : pvorbis_info;

    pcm             : pcmfloat;
    pcmret          : pcmfloat;
    pcm_storage     : int;
    pcm_current     : int;
    pcm_returned    : int;

    preextrapolate  : int;
    eofflag         : int;

    lW              : long;
    W               : long;
    nW              : long;
    centerW         : long;

    granulepos      : ogg_int64_t;
    sequence        : ogg_int64_t;

    glue_bits       : ogg_int64_t;
    time_bits       : ogg_int64_t;
    floor_bits      : ogg_int64_t;
    res_bits        : ogg_int64_t;

    backend_state   : pointer;
  end;

{ vorbis_block is a single block of data to be processed as part of
  the analysis/synthesis stream; it belongs to a specific logical
  bitstream, but is independant from other vorbis_blocks belonging to
  that logical bitstream. }

  palloc_chain = ^alloc_chain;
  alloc_chain = record
    ptr             : pointer;
    next            : palloc_chain;
  end;

  vorbis_block = record
  { necessary stream state for linking to the framing abstraction }
    pcm             : pcmfloat;            { this is a pointer into local storage }
    opb             : oggpack_buffer;

    lW              : long;
    W               : long;
    nW              : long;
    pcmend          : int;
    mode            : int;

    eofflag         : int;
    granulepos      : ogg_int64_t;
    sequence        : ogg_int64_t;
    vd              : pvorbis_dsp_state; { For read-only access of configuration }

  { local storage to avoid remallocing; it's up to the mapping to structure it }
    localstore      : pointer;
    localtop        : long;
    localalloc      : long;
    totaluse        : long;
    reap            : palloc_chain;

  { bitmetrics for the frame }
    glue_bits       : long;
    time_bits       : long;
    floor_bits      : long;
    res_bits        : long;

    internal        : pointer;
  end;

{ vorbis_info contains all the setup information specific to the
  specific compression/decompression mode in progress (eg,
  psychoacoustic settings, channel setup, options, codebook
  etc). vorbis_info and substructures are in backends.h. }

{ the comments are not part of vorbis_info so that vorbis_info can be static storage }

  pcomments = ^comments;
  comments = array[byte] of pchar;

  plengts = ^lengts;
  lengts = array[byte] of int;

  pvorbis_comment = ^vorbis_comment;
  vorbis_comment = record
  { unlimited user comment fields.  libvorbis writes 'libvorbis' whatever vendor is set to in encode }
    user_comments   : pcomments;
    comment_lengths : plengts;
    comments        : int;
    vendor          : pchar;
  end;


{ libvorbis encodes in two abstraction layers; first we perform DSP
  and produce a packet (see docs/analysis.txt).  The packet is then
  coded into a framed OggSquish bitstream by the second layer (see
  docs/framing.txt).  Decode is the reverse process; we sync/frame
  the bitstream and extract individual packets, then decode the
  packet back into PCM audio.

  The extra framing/packetizing is used in streaming formats, such as
  files.  Over the net (such as with UDP), the framing and
  packetization aren't necessary as they're provided by the transport
  and the streaming layer is not used }

{ Vorbis PRIMITIVES: general }

{$IFDEF MSWINDOWS}
const
  vorbislib = 'vorbislib.dll';
{$ENDIF}
{$IFDEF LINUX}
const
  vorbislib = 'libvorbis.so';
{$ENDIF}

procedure vorbis_info_init(var vi: vorbis_info); cdecl; External{$IFDEF DYNLINK} vorbislib name 'vorbis_info_init'{$ENDIF};
procedure vorbis_info_clear(var vi: vorbis_info); cdecl; External{$IFDEF DYNLINK} vorbislib name 'vorbis_info_clear'{$ENDIF};
function  vorbis_info_blocksize(var vi: vorbis_info; zo: int): int; cdecl; External{$IFDEF DYNLINK} vorbislib name 'vorbis_info_blocksize'{$ENDIF};
procedure vorbis_comment_init(var vc: vorbis_comment); cdecl; External{$IFDEF DYNLINK} vorbislib name 'vorbis_comment_init'{$ENDIF};
procedure vorbis_comment_add(var vc: vorbis_comment; comment: pchar); cdecl; External{$IFDEF DYNLINK} vorbislib name 'vorbis_comment_add'{$ENDIF};
procedure vorbis_comment_add_tag(var vc: vorbis_comment; tag: pchar; contents: pchar); cdecl; External{$IFDEF DYNLINK} vorbislib name 'vorbis_comment_add_tag'{$ENDIF};
function  vorbis_comment_query(var vc: vorbis_comment; tag: pchar; count: int): pchar; cdecl; External{$IFDEF DYNLINK} vorbislib name 'vorbis_comment_query'{$ENDIF};
function  vorbis_comment_query_count(var vc: vorbis_comment; tag: pchar): int; cdecl; External{$IFDEF DYNLINK} vorbislib name 'vorbis_comment_query_count'{$ENDIF};
procedure vorbis_comment_clear(var vc: vorbis_comment); cdecl; External{$IFDEF DYNLINK} vorbislib name 'vorbis_comment_clear'{$ENDIF};

function  vorbis_block_init(var v: vorbis_dsp_state; var vb: vorbis_block): int; cdecl; External{$IFDEF DYNLINK} vorbislib name 'vorbis_block_init'{$ENDIF};
function  vorbis_block_clear(var vb: vorbis_block): int; cdecl; External{$IFDEF DYNLINK} vorbislib name 'vorbis_block_clear'{$ENDIF};
procedure vorbis_dsp_clear(var v: vorbis_dsp_state); cdecl; External{$IFDEF DYNLINK} vorbislib name 'vorbis_dsp_clear'{$ENDIF};
function  vorbis_granule_time(var v: vorbis_dsp_state; granulepos: ogg_int64_t): Double; cdecl; External{$IFDEF DYNLINK} vorbislib name 'vorbis_granule_time'{$ENDIF};

{ vorbislib PRIMITIVES: analysis/DSP layer }

function  vorbis_analysis_init(var v: vorbis_dsp_state; var vi: vorbis_info): int; cdecl; External{$IFDEF DYNLINK} vorbislib name 'vorbis_analysis_init'{$ENDIF};
function  vorbis_commentheader_out(var vc: vorbis_comment; var op: ogg_packet): int; cdecl; External{$IFDEF DYNLINK} vorbislib name 'vorbis_commentheader_out'{$ENDIF};
function  vorbis_analysis_headerout(var v:vorbis_dsp_state; var vc: vorbis_comment; var op: ogg_packet;
					  var op_comm: ogg_packet; var op_code: ogg_packet): int; cdecl; External{$IFDEF DYNLINK} vorbislib name 'vorbis_analysis_headerout'{$ENDIF};
function  vorbis_analysis_buffer(var v: vorbis_dsp_state; vals: int): pcmfloat; cdecl; External{$IFDEF DYNLINK} vorbislib name 'vorbis_analysis_buffer'{$ENDIF};
function  vorbis_analysis_wrote(var v: vorbis_dsp_state; vals: int): int; cdecl; External{$IFDEF DYNLINK} vorbislib name 'vorbis_analysis_wrote'{$ENDIF};
function  vorbis_analysis_blockout(var v: vorbis_dsp_state; var vb: vorbis_block): int; cdecl; External{$IFDEF DYNLINK} vorbislib name 'vorbis_analysis_blockout'{$ENDIF};
function  vorbis_analysis(var vb: vorbis_block; var op: ogg_packet): int; cdecl; External{$IFDEF DYNLINK} vorbislib name 'vorbis_analysis'{$ENDIF};

function  vorbis_bitrate_addblock(var vb: vorbis_block): int; cdecl; External{$IFDEF DYNLINK} vorbislib name 'vorbis_bitrate_addblock'{$ENDIF};
function  vorbis_bitrate_flushpacket(var vd: vorbis_dsp_state; var op: ogg_packet): int; cdecl; External{$IFDEF DYNLINK} vorbislib name 'vorbis_bitrate_flushpacket'{$ENDIF};

{ vorbislib PRIMITIVES: synthesis layer }

function  vorbis_synthesis_headerin(var vi: vorbis_info; var vc: vorbis_comment; var op: ogg_packet): int; cdecl; External{$IFDEF DYNLINK} vorbislib name 'vorbis_synthesis_headerin'{$ENDIF};

function  vorbis_synthesis_init(var v: vorbis_dsp_state; var vi: vorbis_info): int; cdecl; External{$IFDEF DYNLINK} vorbislib name 'vorbis_synthesis_init'{$ENDIF};
function  vorbis_synthesis_restart(var v: vorbis_dsp_state): int; cdecl; External{$IFDEF DYNLINK} vorbislib name 'vorbis_synthesis_restart'{$ENDIF};
function  vorbis_synthesis(var vb: vorbis_block; var op: ogg_packet): int; cdecl; External{$IFDEF DYNLINK} vorbislib name 'vorbis_synthesis'{$ENDIF};
function  vorbis_synthesis_trackonly(var vb: vorbis_block; var op: ogg_packet): int; cdecl; External{$IFDEF DYNLINK} vorbislib name 'vorbis_synthesis_trackonly'{$ENDIF};
function  vorbis_synthesis_blockin(var v: vorbis_dsp_state; var vb: vorbis_block): int; cdecl; External{$IFDEF DYNLINK} vorbislib name 'vorbis_synthesis_blockin'{$ENDIF};
function  vorbis_synthesis_pcmout(var v: vorbis_dsp_state; var pcm: pcmfloat): int; cdecl; External{$IFDEF DYNLINK} vorbislib name 'vorbis_synthesis_pcmout'{$ENDIF};
function  vorbis_synthesis_lapout(var v: vorbis_dsp_state; var pcm: pcmfloat): int; cdecl; External{$IFDEF DYNLINK} vorbislib name 'vorbis_synthesis_lapout'{$ENDIF};
function  vorbis_synthesis_read(var v: vorbis_dsp_state; samples: int): int; cdecl; External{$IFDEF DYNLINK} vorbislib name 'vorbis_synthesis_read'{$ENDIF};
function  vorbis_packet_blocksize(var vi: vorbis_info; var op: ogg_packet): long; cdecl; External{$IFDEF DYNLINK} vorbislib name 'vorbis_packet_blocksize'{$ENDIF};

function  vorbis_synthesis_halfrate(var v: vorbis_info; flag: int): int; cdecl; External{$IFDEF DYNLINK} vorbislib name 'vorbis_synthesis_halfrate'{$ENDIF};
function  vorbis_synthesis_halfrate_p(var v: vorbis_info): int; cdecl; External{$IFDEF DYNLINK} vorbislib name 'vorbis_synthesis_halfrate_p'{$ENDIF};

{ vorbislib ERRORS and return codes }
Const
  OV_FALSE          = -1;
  OV_EOF            = -2;
  OV_HOLE           = -3;

  OV_EREAD          = -128;
  OV_EFAULT         = -129;
  OV_EIMPL          = -130;
  OV_EINVAL         = -131;
  OV_ENOTVORBIS     = -132;
  OV_EBADHEADER     = -133;
  OV_EVERSION       = -134;
  OV_ENOTAUDIO      = -135;
  OV_EBADPACKET     = -136;
  OV_EBADLINK       = -137;
  OV_ENOSEEK        = -138;


(***********************************************************************)
(* Header : vorbisfile.h                                               *)
(***********************************************************************)

type

{* The function prototypes for the callbacks are basically the same as for

 * the stdio functions fread, fseek, fclose, ftell.
 * The one difference is that the FILE * arguments have been replaced with
 * a void * - this is to be used as a pointer to whatever internal data these
 * functions might need. In the stdio case, it's just a FILE * cast to a void *
 *
 * If you use other functions, check the docs for these functions and return
 * the right values. For seek_func(), you *MUST* return -1 if the stream is
 * unseekable
 *}

  read_func  = function(ptr: pointer; size, nmemb: Longword; datasource: pointer): LongWord; cdecl;
  seek_func  = function(datasource: pointer; offset: ogg_int64_t; whence: int): int; cdecl;
  close_func = function(datasource: pointer): int; cdecl;
  tell_func  = function(datasource: pointer): long; cdecl;

  ov_callbacks = record
    read            : read_func;
    seek            : seek_func;
    close           : close_func;
    tell            : tell_func;
  end;

const
  NOTOPEN           = 0;
  PARTOPEN          = 1;
  OPENED            = 2;
  STREAMSET         = 3;
  INITSET           = 4;

type
  OggVorbis_File = record
    datasource      : pointer; { pointer to a FILE *, etc. }
    seekable        : int;
    offset          : ogg_int64_t;
    end_            : ogg_int64_t;
    oy              : ogg_sync_state;

  { If the FILE handle isn't seekable (eg, a pipe), only the current stream appears }
    links           : int;
    offsets         : ^ogg_int64_t;
    dataoffsets     : ^ogg_int64_t;
    serialnos       : ^long;
    pcmlengths      : ^ogg_int64_t; { overloaded to maintain binary compatability; x2 size, stores both beginning and end values }
    vi              : pvorbis_info;
    vc              : pvorbis_comment;

  { Decoding working state local storage }
    pcm_offset      : ogg_int64_t;
    ready_state     : int;
    current_serialno: long;
    current_link    : int;

    bittrack        : double;
    samptrack       : double;

    os              : ogg_stream_state; { take physical pages, weld into a logical stream of packets }
    vd              : vorbis_dsp_state; { central working state for the packet->PCM decoder }
    vb              : vorbis_block;     { local working space for packet->PCM decode }

    callbacks       : ov_callbacks;
  end;

{$IFDEF MSWINDOWS}
const
  vorbisfilelib = 'vorbisfile.dll';
{$ENDIF}
{$IFDEF LINUX}
const
  vorbisfilelib = 'libvorbisfile.so';
{$ENDIF}

function ov_clear(var vf: OggVorbis_File): int; cdecl; External{$IFDEF DYNLINK} vorbisfilelib name 'ov_clear'{$ENDIF};
function ov_open(f: pointer; var vf: OggVorbis_File; initial: pointer; ibytes: long): int; cdecl; External{$IFDEF DYNLINK} vorbisfilelib name 'ov_open'{$ENDIF};
function ov_open_callbacks(datasource: pointer; var vf: OggVorbis_File; initial: pointer; ibytes: long; callbacks: ov_callbacks): int; cdecl; External{$IFDEF DYNLINK} vorbisfilelib name 'ov_open_callbacks'{$ENDIF};

function ov_test(f: pointer; var vf: OggVorbis_File; initial: pointer; ibytes: long): int; cdecl; External{$IFDEF DYNLINK} vorbisfilelib name 'ov_test'{$ENDIF};
function ov_test_callbacks(datasource: pointer; var vf: OggVorbis_File; initial: pointer; ibytes: long; callbacks: ov_callbacks): int; cdecl; External{$IFDEF DYNLINK} vorbisfilelib name 'ov_test_callbacks'{$ENDIF};
function ov_test_open(var vf: OggVorbis_File): int; cdecl; External{$IFDEF DYNLINK} vorbisfilelib name 'ov_test_open'{$ENDIF};

function ov_bitrate(var vf: OggVorbis_File; i: int): long; cdecl; External{$IFDEF DYNLINK} vorbisfilelib name 'ov_bitrate'{$ENDIF};
function ov_bitrate_instant(var vf: OggVorbis_File): long; cdecl; External{$IFDEF DYNLINK} vorbisfilelib name 'ov_bitrate_instant'{$ENDIF};
function ov_streams(var vf: OggVorbis_File): long; cdecl; External{$IFDEF DYNLINK} vorbisfilelib name 'ov_streams'{$ENDIF};
function ov_seekable(var vf: OggVorbis_File): long; cdecl; External{$IFDEF DYNLINK} vorbisfilelib name 'ov_seekable'{$ENDIF};
function ov_serialnumber(var vf: OggVorbis_File; i: int): long; cdecl; External{$IFDEF DYNLINK} vorbisfilelib name 'ov_serialnumber'{$ENDIF};

function ov_raw_total(var vf: OggVorbis_File; i: int): ogg_int64_t; cdecl; External{$IFDEF DYNLINK} vorbisfilelib name 'ov_raw_total'{$ENDIF};
function ov_pcm_total(var vf: OggVorbis_File; i: int): ogg_int64_t; cdecl; External{$IFDEF DYNLINK} vorbisfilelib name 'ov_pcm_total'{$ENDIF};
function ov_time_total(var vf: OggVorbis_File; i: int): Double; cdecl; External{$IFDEF DYNLINK} vorbisfilelib name 'ov_time_total'{$ENDIF};

function ov_raw_seek(var vf: OggVorbis_File; pos: ogg_int64_t): int; cdecl; External{$IFDEF DYNLINK} vorbisfilelib name 'ov_raw_seek'{$ENDIF};
function ov_pcm_seek(var vf: OggVorbis_File; pos: ogg_int64_t): int; cdecl; External{$IFDEF DYNLINK} vorbisfilelib name 'ov_pcm_seek'{$ENDIF};
function ov_pcm_seek_page(var vf: OggVorbis_File; pos: ogg_int64_t): int; cdecl; External{$IFDEF DYNLINK} vorbisfilelib name 'ov_pcm_seek_page'{$ENDIF};
function ov_time_seek(var vf: OggVorbis_File; pos: double): int; cdecl; External{$IFDEF DYNLINK} vorbisfilelib name 'ov_time_seek'{$ENDIF};
function ov_time_seek_page(var vf: OggVorbis_File; pos: double): int; cdecl; External{$IFDEF DYNLINK} vorbisfilelib name 'ov_time_seek_page'{$ENDIF};

function ov_raw_seek_lap(var vf: OggVorbis_File; pos: ogg_int64_t): int; cdecl; External{$IFDEF DYNLINK} vorbisfilelib name 'ov_raw_seek_lap'{$ENDIF};
function ov_pcm_seek_lap(var vf: OggVorbis_File; pos: ogg_int64_t): int; cdecl; External{$IFDEF DYNLINK} vorbisfilelib name 'ov_pcm_seek_lap'{$ENDIF};
function ov_pcm_seek_page_lap(var vf: OggVorbis_File; pos: ogg_int64_t): int; cdecl; External{$IFDEF DYNLINK} vorbisfilelib name 'ov_pcm_seek_page_lap'{$ENDIF};
function ov_time_seek_lap(var vf: OggVorbis_File; pos: double): int; cdecl; External{$IFDEF DYNLINK} vorbisfilelib name 'ov_time_seek_lap'{$ENDIF};
function ov_time_seek_page_lap(var vf: OggVorbis_File; pos: double): int; cdecl; External{$IFDEF DYNLINK} vorbisfilelib name 'ov_time_seek_page_lap'{$ENDIF};

function ov_raw_tell(var vf: OggVorbis_File): ogg_int64_t; cdecl; External{$IFDEF DYNLINK} vorbisfilelib name 'ov_raw_tell'{$ENDIF};
function ov_pcm_tell(var vf: OggVorbis_File): ogg_int64_t; cdecl; External{$IFDEF DYNLINK} vorbisfilelib name 'ov_pcm_tell'{$ENDIF};
function ov_time_tell(var vf: OggVorbis_File): Double; cdecl; External{$IFDEF DYNLINK} vorbisfilelib name 'ov_time_tell'{$ENDIF};

function ov_info(var vf: OggVorbis_File; link: int): pvorbis_info; cdecl; External{$IFDEF DYNLINK} vorbisfilelib name 'ov_info'{$ENDIF};
function ov_comment(var vf: OggVorbis_File; link: int): pvorbis_comment; cdecl; External{$IFDEF DYNLINK} vorbisfilelib name 'ov_comment'{$ENDIF};

function ov_read_float(var vf: OggVorbis_File; var pcm_channels: pcmfloat; samples: int; bitstream: pint): long; cdecl; External{$IFDEF DYNLINK} vorbisfilelib name 'ov_read_float'{$ENDIF};
function ov_read(var vf: OggVorbis_File; buffer: pointer; length, bigendianp, word, sgned: int; bitstream: Pint): long; cdecl; External{$IFDEF DYNLINK} vorbisfilelib name 'ov_read'{$ENDIF};
function ov_crosslap(var vf1: OggVorbis_File; var vf2: OggVorbis_File): int; cdecl; External{$IFDEF DYNLINK} vorbisfilelib name 'ov_crosslap'{$ENDIF};

function ov_halfrate(var vf: OggVorbis_File; flag: int): int; cdecl; External{$IFDEF DYNLINK} vorbisfilelib name 'ov_halfrate'{$ENDIF};
function ov_halfrate_p(var vf: OggVorbis_File): int; cdecl; External{$IFDEF DYNLINK} vorbisfilelib name 'ov_halfrate_p'{$ENDIF};


(***********************************************************************)
(* Header : vorbisenc.h                                                *)
(***********************************************************************)

{$IFDEF MSWINDOWS}
const
  vorbisenclib = 'vorbisenclib.dll';
{$ENDIF}
{$IFDEF LINUX}
const
  vorbisenclib = 'libvorbisenc.so';
{$ENDIF}

const
  OV_ECTL_RATEMANAGE_GET       = $10;

  OV_ECTL_RATEMANAGE_SET       = $11;
  OV_ECTL_RATEMANAGE_AVG       = $12;
  OV_ECTL_RATEMANAGE_HARD      = $13;

  OV_ECTL_LOWPASS_GET          = $20;
  OV_ECTL_LOWPASS_SET          = $21;

  OV_ECTL_IBLOCK_GET           = $30;
  OV_ECTL_IBLOCK_SET           = $31;

type
  ovectl_ratemanage_arg = record
    management_active        : int;

    bitrate_hard_min         : long;
    bitrate_hard_max         : long;
    bitrate_hard_window      : double;

    bitrate_av_lo            : long;
    bitrate_av_hi            : long;
    bitrate_av_window        : double;
    bitrate_av_window_center : double;
  end;

function vorbis_encode_init(var vi: vorbis_info; channels, rate, max_bitrate, nominal_bitrate, min_bitrate: long): int; cdecl; External{$IFDEF DYNLINK} vorbisenclib name 'vorbis_encode_init'{$ENDIF};
function vorbis_encode_setup_managed(var vi: vorbis_info; channels, rate, max_bitrate, nominal_bitrate, min_bitrate: long): int; cdecl; External{$IFDEF DYNLINK} vorbisenclib name 'vorbis_encode_setup_managed'{$ENDIF};
function vorbis_encode_setup_vbr(var vi: vorbis_info; channels, rate: long; quality: float): int; cdecl; External{$IFDEF DYNLINK} vorbisenclib name 'vorbis_encode_setup_vbr'{$ENDIF};
(* quality level from 0. (lo) to 1. (hi) *)
function vorbis_encode_init_vbr(var vi: vorbis_info; channels, rate: long; base_quality: float): int; cdecl; External{$IFDEF DYNLINK} vorbisenclib name 'vorbis_encode_init_vbr'{$ENDIF};
function vorbis_encode_setup_init(var vi: vorbis_info): int; cdecl; External{$IFDEF DYNLINK} vorbisenclib name 'vorbis_encode_setup_init'{$ENDIF};
function vorbis_encode_ctl(var vi: vorbis_info; number: int; arg: pointer): int; cdecl; External{$IFDEF DYNLINK} vorbisenclib name 'vorbis_encode_ctl'{$ENDIF};

implementation

end.

