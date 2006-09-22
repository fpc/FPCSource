{
  Translation of the ogg headers for FreePascal
  Copyright (C) 2006 by Ivo Steinmann
}

(********************************************************************
 *                                                                  *
 * THIS FILE IS PART OF THE OggVorbis SOFTWARE CODEC SOURCE CODE.   *
 * USE, DISTRIBUTION AND REPRODUCTION OF THIS LIBRARY SOURCE IS     *
 * GOVERNED BY A BSD-STYLE SOURCE LICENSE INCLUDED WITH THIS SOURCE *
 * IN 'COPYING'. PLEASE READ THESE TERMS BEFORE DISTRIBUTING.       *
 *                                                                  *
 * THE OggVorbis SOURCE CODE IS (C) COPYRIGHT 1994-2002             *
 * by the Xiph.Org Foundation http://www.xiph.org/                  *
 *                                                                  *
 ********************************************************************)

unit ogg;

{$mode objfpc}
{$MINENUMSIZE 4}

interface

uses
  ctypes;

{$IFDEF WINDOWS}
  {$DEFINE DYNLINK}
{$ENDIF}

{$IFDEF DYNLINK}
const
{$IF Defined(WINDOWS)}
  ogglib = 'ogglib.dll';
{$ELSEIF Defined(UNIX)}
  ogglib = 'libogg.so';
{$ELSE}
  {$MESSAGE ERROR 'DYNLINK not supported'}
{$IFEND}
{$ELSE}
  {$LINKLIB ogg}
{$ENDIF}


(***********************************************************************)
(* Header : os_types.h                                                 *)
(***********************************************************************)
type
  ogg_int64_t    = cint64;              pogg_int64_t    = ^ogg_int64_t;
  ogg_int32_t    = cint32;              pogg_int32_t    = ^ogg_int32_t;
  ogg_uint32_t   = cuint32;             pogg_uint32_t   = ^ogg_uint32_t;
  ogg_int16_t    = cint16;              pogg_int16_t    = ^ogg_int16_t;
  ogg_uint16_t   = cuint16;             pogg_uint16_t   = ^ogg_uint16_t;


(***********************************************************************)
(* Header : ogg.h                                                      *)
(***********************************************************************)
type
  poggpack_buffer = ^oggpack_buffer;
  oggpack_buffer = record
    endbyte         : clong;
    endbit          : cint;
    buffer          : pcuchar;
    ptr             : pcuchar;
    storage         : clong;
  end;

{ ogg_page is used to encapsulate the data in one Ogg bitstream page }

  pogg_page = ^ogg_page;
  ogg_page = record
    header          : pcuchar;
    header_len      : clong;
    body            : pcuchar;
    body_len        : clong;
  end;

{ ogg_stream_state contains the current encode/decode state of a logical Ogg bitstream }

  pogg_stream_state = ^ogg_stream_state;
  ogg_stream_state = record
    body_data       : pcuchar;                 { bytes from packet bodies }
    body_storage    : clong;                           { storage elements allocated }
    body_fill       : clong;                           { elements stored; fill mark }
    body_returned   : clong;                           { elements of fill returned }

    lacing_vals     : pcint;                            { The values that will go to the segment table }
    granule_vals    : pogg_int64_t;                    { granulepos values for headers. Not compact this way, but it is simple coupled to the lacing fifo }

    lacing_storage  : clong;
    lacing_fill     : clong;
    lacing_packet   : clong;
    lacing_returned : clong;

    header          : array[0..281] of cuchar; { working space for header encode }
    header_fill     : cint;

    e_o_s           : cint;                            { set when we have buffered the last packet in the logical bitstream }
    b_o_s           : cint;                            { set after we've written the initial page of a logical bitstream }

    serialno        : clong;
    pageno          : clong;
    packetno        : ogg_int64_t;                    { sequence number for decode; the framing knows where there's a hole in the data,
                                                        but we need coupling so that the codec (which is in a seperate abstraction layer) also knows about the gap }
    granulepos      : ogg_int64_t;
  end;

{ ogg_packet is used to encapsulate the data and metadata belonging to a single raw Ogg/Vorbis packet }

  pogg_packet = ^ogg_packet;
  ogg_packet = record
    packet          : pcuchar;
    bytes           : clong;
    b_o_s           : clong;
    e_o_s           : clong;

    granulepos      : ogg_int64_t;
    packetno        : ogg_int64_t;             { sequence number for decode; the framing knows where there's a hole in the data,
                                                 but we need coupling so that the codec (which is in a seperate abstraction layer) also knows about the gap }
  end;

  ogg_sync_state = record
    data            : pcuchar;
    storage         : cint;
    fill            : cint;
    returned        : cint;

    unsynced        : cint;
    headerbytes     : cint;
    bodybytes       : cint;
  end;

{ Ogg BITSTREAM PRIMITIVES: bitstream }

procedure oggpack_writeinit(var b: oggpack_buffer); cdecl; external {$IFDEF DYNLINK}ogglib{$ENDIF};
procedure oggpack_writetrunc(var b: oggpack_buffer; bits: clong); cdecl; external {$IFDEF DYNLINK}ogglib{$ENDIF};
procedure oggpack_writealign(var b: oggpack_buffer); cdecl; external {$IFDEF DYNLINK}ogglib{$ENDIF};
procedure oggpack_writecopy(var b: oggpack_buffer; source: pointer; bits: clong); cdecl; external {$IFDEF DYNLINK}ogglib{$ENDIF};
procedure oggpack_reset(var b: oggpack_buffer); cdecl; external {$IFDEF DYNLINK}ogglib{$ENDIF};
procedure oggpack_writeclear(var b: oggpack_buffer); cdecl; external {$IFDEF DYNLINK}ogglib{$ENDIF};
procedure oggpack_readinit(var b: oggpack_buffer; buf: pointer; bytes: cint); cdecl; external {$IFDEF DYNLINK}ogglib{$ENDIF};
procedure oggpack_write(var b: oggpack_buffer; value: culong; bits: cint); cdecl; external {$IFDEF DYNLINK}ogglib{$ENDIF};
function  oggpack_look(var b: oggpack_buffer; bits: cint): clong; cdecl; external {$IFDEF DYNLINK}ogglib{$ENDIF};
function  oggpack_look1(var b: oggpack_buffer): clong; cdecl; external {$IFDEF DYNLINK}ogglib{$ENDIF};
procedure oggpack_adv(var b: oggpack_buffer; bits: cint); cdecl; external {$IFDEF DYNLINK}ogglib{$ENDIF};
procedure oggpack_adv1(var b: oggpack_buffer); cdecl; external {$IFDEF DYNLINK}ogglib{$ENDIF};
function  oggpack_read(var b: oggpack_buffer; bits: cint): clong; cdecl; external {$IFDEF DYNLINK}ogglib{$ENDIF};
function  oggpack_read1(var b: oggpack_buffer): clong; cdecl; external {$IFDEF DYNLINK}ogglib{$ENDIF};
function  oggpack_bytes(var b: oggpack_buffer): clong; cdecl; external {$IFDEF DYNLINK}ogglib{$ENDIF};
function  oggpack_bits(var b: oggpack_buffer): clong; cdecl; external {$IFDEF DYNLINK}ogglib{$ENDIF};
function  oggpack_get_buffer(var b: oggpack_buffer): pointer; cdecl; external {$IFDEF DYNLINK}ogglib{$ENDIF};

procedure oggpackB_writeinit(var b: oggpack_buffer); cdecl; external {$IFDEF DYNLINK}ogglib{$ENDIF};
procedure oggpackB_writetrunc(var b: oggpack_buffer; bits: clong); cdecl; external {$IFDEF DYNLINK}ogglib{$ENDIF};
procedure oggpackB_writealign(var b: oggpack_buffer); cdecl; external {$IFDEF DYNLINK}ogglib{$ENDIF};
procedure oggpackB_writecopy(var b: oggpack_buffer; source: pointer; bits: clong); cdecl; external {$IFDEF DYNLINK}ogglib{$ENDIF};
procedure oggpackB_reset(var b: oggpack_buffer); cdecl; external {$IFDEF DYNLINK}ogglib{$ENDIF};
procedure oggpackB_writeclear(var b: oggpack_buffer); cdecl; external {$IFDEF DYNLINK}ogglib{$ENDIF};
procedure oggpackB_readinit(var b: oggpack_buffer; buf: pointer; bytes: cint); cdecl; external {$IFDEF DYNLINK}ogglib{$ENDIF};
procedure oggpackB_write(var b: oggpack_buffer; value: culong; bits: cint); cdecl; external {$IFDEF DYNLINK}ogglib{$ENDIF};
function  oggpackB_look(var b: oggpack_buffer; bits: cint): clong; cdecl; external {$IFDEF DYNLINK}ogglib{$ENDIF};
function  oggpackB_look1(var b: oggpack_buffer): clong; cdecl; external {$IFDEF DYNLINK}ogglib{$ENDIF};
procedure oggpackB_adv(var b: oggpack_buffer; bits: cint); cdecl; external {$IFDEF DYNLINK}ogglib{$ENDIF};
procedure oggpackB_adv1(var b: oggpack_buffer); cdecl; external {$IFDEF DYNLINK}ogglib{$ENDIF};
function  oggpackB_read(var b: oggpack_buffer; bits: cint): clong; cdecl; external {$IFDEF DYNLINK}ogglib{$ENDIF};
function  oggpackB_read1(var b: oggpack_buffer): clong; cdecl; external {$IFDEF DYNLINK}ogglib{$ENDIF};
function  oggpackB_bytes(var b: oggpack_buffer): clong; cdecl; external {$IFDEF DYNLINK}ogglib{$ENDIF};
function  oggpackB_bits(var b: oggpack_buffer): clong; cdecl; external {$IFDEF DYNLINK}ogglib{$ENDIF};
function  oggpackB_get_buffer(var b: oggpack_buffer): pointer; cdecl; external {$IFDEF DYNLINK}ogglib{$ENDIF};

{ ogglib BITSTREAM PRIMITIVES: encoding }

function  ogg_stream_packetin(var os: ogg_stream_state; var op: ogg_packet): cint; cdecl; external {$IFDEF DYNLINK}ogglib{$ENDIF};
function  ogg_stream_pageout(var os: ogg_stream_state; var op: ogg_page): cint; cdecl; external {$IFDEF DYNLINK}ogglib{$ENDIF};
function  ogg_stream_flush(var os: ogg_stream_state; var op: ogg_page): cint; cdecl; external {$IFDEF DYNLINK}ogglib{$ENDIF};

{ ogglib BITSTREAM PRIMITIVES: decoding }

function  ogg_sync_init(var oy: ogg_sync_state): cint; cdecl; external {$IFDEF DYNLINK}ogglib{$ENDIF};
function  ogg_sync_clear(var oy: ogg_sync_state): cint; cdecl; external {$IFDEF DYNLINK}ogglib{$ENDIF};
function  ogg_sync_reset(var oy: ogg_sync_state): cint; cdecl; external {$IFDEF DYNLINK}ogglib{$ENDIF};
function  ogg_sync_destroy(var oy: ogg_sync_state): cint; cdecl; external {$IFDEF DYNLINK}ogglib{$ENDIF};

function  ogg_sync_buffer(var oy: ogg_sync_state; size: clong): pointer; cdecl; external {$IFDEF DYNLINK}ogglib{$ENDIF};
function  ogg_sync_wrote(var oy: ogg_sync_state; bytes: clong): cint; cdecl; external {$IFDEF DYNLINK}ogglib{$ENDIF};
function  ogg_sync_pageseek(var oy: ogg_sync_state; var og: ogg_page): pointer; cdecl; external {$IFDEF DYNLINK}ogglib{$ENDIF};
function  ogg_sync_pageout(var oy: ogg_sync_state; var og: ogg_page): cint; cdecl; external {$IFDEF DYNLINK}ogglib{$ENDIF};
function  ogg_stream_pagein(var os: ogg_stream_state; var og: ogg_page): cint; cdecl; external {$IFDEF DYNLINK}ogglib{$ENDIF};
function  ogg_stream_packetout(var os: ogg_stream_state; var op: ogg_packet): cint; cdecl; external {$IFDEF DYNLINK}ogglib{$ENDIF};
function  ogg_stream_packetpeek(var os: ogg_stream_state; var op: ogg_packet): cint; cdecl; external {$IFDEF DYNLINK}ogglib{$ENDIF};

{ ogglib BITSTREAM PRIMITIVES: general }

function  ogg_stream_init(var os: ogg_stream_state; serialno: cint): cint; cdecl; external {$IFDEF DYNLINK}ogglib{$ENDIF};
function  ogg_stream_clear(var os: ogg_stream_state): cint; cdecl; external {$IFDEF DYNLINK}ogglib{$ENDIF};
function  ogg_stream_reset(var os: ogg_stream_state): cint; cdecl; external {$IFDEF DYNLINK}ogglib{$ENDIF};
function  ogg_stream_reset_serialno(var os: ogg_stream_state; serialno: cint): cint; cdecl; external {$IFDEF DYNLINK}ogglib{$ENDIF};
function  ogg_stream_destroy(var os: ogg_stream_state): cint; cdecl; external {$IFDEF DYNLINK}ogglib{$ENDIF};
function  ogg_stream_eos(var os: ogg_stream_state): cint; cdecl; external {$IFDEF DYNLINK}ogglib{$ENDIF};

procedure ogg_page_checksum_set(var og: ogg_page); cdecl; external {$IFDEF DYNLINK}ogglib{$ENDIF};

function  ogg_page_version(var og: ogg_page): cint; cdecl; external {$IFDEF DYNLINK}ogglib{$ENDIF};
function  ogg_page_continued(var og: ogg_page): cint; cdecl; external {$IFDEF DYNLINK}ogglib{$ENDIF};
function  ogg_page_bos(var og: ogg_page): cint; cdecl; external {$IFDEF DYNLINK}ogglib{$ENDIF};
function  ogg_page_eos(var og: ogg_page): cint; cdecl; external {$IFDEF DYNLINK}ogglib{$ENDIF};
function  ogg_page_granulepos(var og: ogg_page): ogg_int64_t; cdecl; external {$IFDEF DYNLINK}ogglib{$ENDIF};
function  ogg_page_serialno(var og: ogg_page): cint; cdecl; external {$IFDEF DYNLINK}ogglib{$ENDIF};
function  ogg_page_pageno(var og: ogg_page): clong; cdecl; external {$IFDEF DYNLINK}ogglib{$ENDIF};
function  ogg_page_packets(var og: ogg_page): cint; cdecl; external {$IFDEF DYNLINK}ogglib{$ENDIF};

procedure ogg_packet_clear(var op: ogg_packet); cdecl; external {$IFDEF DYNLINK}ogglib{$ENDIF};

implementation

end.
