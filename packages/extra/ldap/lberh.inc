(* $OpenLDAP: pkg/ldap/include/lber.h,v 1.83.2.11 2005/01/20 17:00:58 kurt Exp $ *)
(* This work is part of OpenLDAP Software <http://www.openldap.org/>.
 *
 * Copyright 1998-2005 The OpenLDAP Foundation.
 * All rights reserved.
 *
 * Redistribution and use in source and binary forms, with or without
 * modification, are permitted only as authorized by the OpenLDAP
 * Public License.
 *
 * A copy of this license is available in file LICENSE in the
 * top-level directory of the distribution or, alternatively, at
 * <http://www.OpenLDAP.org/license.html>.
 *)
(* Portions Copyright (c) 1990 Regents of the University of Michigan.
 * All rights reserved.
 *
 * Redistribution and use in source and binary forms are permitted
 * provided that this notice is preserved and that due credit is given
 * to the University of Michigan at Ann Arbor. The name of the University
 * may not be used to endorse or promote products derived from this
 * software without specific prior written permission. This software
 * is provided ``as is'' without express or implied warranty.
 *)

(* Overview of LBER tag construction
 *
 *  Bits
 *  ______
 *  8 7 | CLASS
 *  0 0 = UNIVERSAL
 *  0 1 = APPLICATION
 *  1 0 = CONTEXT-SPECIFIC
 *  1 1 = PRIVATE
 *      _____
 *      | 6 | DATA-TYPE
 *        0 = PRIMITIVE
 *        1 = CONSTRUCTED
 *          ___________
 *          | 5 ... 1 | TAG-NUMBER
 *)

const
(* BER classes and mask *)
  LBER_CLASS_UNIVERSAL              = ber_tag_t($00);
  LBER_CLASS_APPLICATION            = ber_tag_t($40);
  LBER_CLASS_CONTEXT                = ber_tag_t($80);
  LBER_CLASS_PRIVATE                = ber_tag_t($c0);
  LBER_CLASS_MASK                   = ber_tag_t($c0);

(* BER encoding type and mask *)
  LBER_PRIMITIVE                    = ber_tag_t($00);
  LBER_CONSTRUCTED                  = ber_tag_t($20);
  LBER_ENCODING_MASK                = ber_tag_t($20);

  LBER_BIG_TAG_MASK                 = ber_tag_t($1f);
  LBER_MORE_TAG_MASK                = ber_tag_t($80);

(*
 * Note that LBER_ERROR and LBER_DEFAULT are values that can never appear
 * as valid BER tags, and so it is safe to use them to report errors.  In
 * fact, any tag for which the following is true is invalid:
 *)
function LBER_INVALID(t: ber_tag_t): ber_tag_t;

const
  LBER_ERROR                        = ber_tag_t(-1);
  LBER_DEFAULT                      = ber_tag_t(-1);

(* general BER types we know about *)
  LBER_BOOLEAN                      = ber_tag_t($01);
  LBER_INTEGER                      = ber_tag_t($02);
  LBER_BITSTRING                    = ber_tag_t($03);
  LBER_OCTETSTRING                  = ber_tag_t($04);
  LBER_NULL                         = ber_tag_t($05);
  LBER_ENUMERATED                   = ber_tag_t($0a);
  LBER_SEQUENCE                     = ber_tag_t($30);    (* constructed *)
  LBER_SET                          = ber_tag_t($31);    (* constructed *)

(* LBER BerElement options *)
  LBER_USE_DER                      = $01;

(* get/set options for BerElement *)
  LBER_OPT_BER_OPTIONS              = $01;
  LBER_OPT_BER_DEBUG                = $02;
  LBER_OPT_BER_REMAINING_BYTES      = $03;
  LBER_OPT_BER_TOTAL_BYTES          = $04;
  LBER_OPT_BER_BYTES_TO_WRITE       = $05;
  LBER_OPT_BER_MEMCTX               = $06;

  LBER_OPT_DEBUG_LEVEL              = LBER_OPT_BER_DEBUG;
  LBER_OPT_REMAINING_BYTES          = LBER_OPT_BER_REMAINING_BYTES;
  LBER_OPT_TOTAL_BYTES              = LBER_OPT_BER_TOTAL_BYTES;
  LBER_OPT_BYTES_TO_WRITE           = LBER_OPT_BER_BYTES_TO_WRITE;

  LBER_OPT_LOG_PRINT_FN             = $8001;
  LBER_OPT_MEMORY_FNS               = $8002;
  LBER_OPT_ERROR_FN                 = $8003;
  LBER_OPT_LOG_PRINT_FILE           = $8004;

(* get/set Memory Debug options *)
  LBER_OPT_MEMORY_INUSE             = $8005;  (* for memory debugging *)
  LBER_OPT_LOG_PROC                 = $8006;  (* for external logging function *)


type
  BER_ERRNO_FN                      = function: pcint; cdecl;

  BER_LOG_PRINT_FN                  = procedure(const buf: pcchar); cdecl;

  BER_MEMALLOC_FN                   = function(size: ber_len_t): Pointer; cdecl;
  BER_MEMCALLOC_FN                  = function(n: ber_len_t; size: ber_len_t): Pointer; cdecl;
  BER_MEMREALLOC_FN                 = function(p: Pointer; size: ber_len_t): Pointer; cdecl;
  BER_MEMFREE_FN                    = function(p: Pointer): Pointer; cdecl;

  plber_memory_fns                  = ^lber_memory_fns;
  lber_memory_fns                   = record
    bmf_malloc  : BER_MEMALLOC_FN;
    bmf_calloc  : BER_MEMCALLOC_FN;
    bmf_realloc : BER_MEMREALLOC_FN;
    bmf_free    : BER_MEMFREE_FN;
  end;

  PBerMemoryFunctions               = ^BerMemoryFunctions;
  BerMemoryFunctions                = lber_memory_fns;

const
(* LBER Sockbuf_IO options *)
  LBER_SB_OPT_GET_FD                = 1;
  LBER_SB_OPT_SET_FD                = 2;
  LBER_SB_OPT_HAS_IO                = 3;
  LBER_SB_OPT_SET_NONBLOCK          = 4;
  LBER_SB_OPT_GET_SSL               = 7;
  LBER_SB_OPT_DATA_READY            = 8;
  LBER_SB_OPT_SET_READAHEAD         = 9;
  LBER_SB_OPT_DRAIN                 = 10;
  LBER_SB_OPT_NEEDS_READ            = 11;
  LBER_SB_OPT_NEEDS_WRITE           = 12;
  LBER_SB_OPT_GET_MAX_INCOMING      = 13;
  LBER_SB_OPT_SET_MAX_INCOMING      = 14;
(* Largest option used by the library *)
  LBER_SB_OPT_OPT_MAX               = 14;

(* LBER IO operations stacking levels *)
  LBER_SBIOD_LEVEL_PROVIDER         = 10;
  LBER_SBIOD_LEVEL_TRANSPORT        = 20;
  LBER_SBIOD_LEVEL_APPLICATION      = 30;

(* get/set options for Sockbuf *)
  LBER_OPT_SOCKBUF_DESC             = $1000;
  LBER_OPT_SOCKBUF_OPTIONS          = $1001;
  LBER_OPT_SOCKBUF_DEBUG            = $1002;

  LBER_OPT_SUCCESS                  =  0;
  LBER_OPT_ERROR                    = -1;

(* on/off values *)
const
  LBER_OPT_OFF                      = Pointer(0);

var
  ber_pvt_opt_on: cchar; cvar; external;

function LBER_OPT_ON: Pointer;

(* Structure for LBER IO operarion descriptor *)
type
  PBerElement                       = Pointer;

  PSockbuf                          = Pointer;

  PSeqorset                         = Pointer;

  PSockbuf_IO                       = ^Sockbuf_IO;

  PSockbuf_IO_Desc                  = ^Sockbuf_IO_Desc;
  Sockbuf_IO_Desc                   = record
    sbiod_level     : cint;
    sbiod_sb        : PSockbuf;
    sbiod_io        : PSockbuf_IO;
    sbiod_pvt       : Pointer;
    sbiod_next      : PSockbuf_IO_Desc;
  end;

(* Structure for LBER IO operation functions *)
  Sockbuf_IO                        = record
    sbi_setup       : function(sbiod: PSockbuf_IO_Desc; arg: Pointer): cint; cdecl;
    sbi_remove      : function(sbiod: PSockbuf_IO_Desc): cint; cdecl;
    sbi_ctrl        : function(sbiod: PSockbuf_IO_Desc; opt: cint; arg: Pointer): cint; cdecl;
    sbi_read        : function(sbiod: PSockbuf_IO_Desc; buf: Pointer; len: ber_len_t): ber_slen_t; cdecl;
    sbi_write       : function(sbiod: PSockbuf_IO_Desc; buf: Pointer; len: ber_len_t): ber_slen_t; cdecl;
    sbi_close       : function(sbiod: PSockbuf_IO_Desc): cint; cdecl;
  end;

(* Helper macros for LBER IO functions *)
function LBER_SBIOD_READ_NEXT(sbiod: PSockbuf_IO_Desc; buf: Pointer; len: ber_len_t): ber_slen_t;

function LBER_SBIOD_WRITE_NEXT(sbiod: PSockbuf_IO_Desc; buf: Pointer; len: ber_len_t): ber_slen_t;

function LBER_SBIOD_CTRL_NEXT(sbiod: PSockbuf_IO_Desc; opt: cint; arg: Pointer): cint;


(* structure for returning a sequence of octet strings + length *)
type
  PPPBerval                         = ^PPBerval;
  PPBerval                          = ^PBerval;
  PBerval                           = ^Berval;
  Berval                            = record
    bv_len  : ber_len_t;
    bv_val  : pcchar;
  end;
  
  PBerVarray                        = ^BerVarray;
  BerVarray                         = ^PBerval;  (* To distinguish from a single bv *)


(* this should be moved to lber-cint.h *)

(*
 * in bprint.c:
 *)
procedure ber_error_print(const data: pcchar); cdecl; external;

procedure ber_bprint(const data: pcchar; len: ber_len_t); cdecl; external;

procedure ber_dump(ber: PBerElement; inout : cint); cdecl; external;

procedure ber_sos_dump(sos: PSeqorset); cdecl; external;


(*
 * in decode.c:
 *)
type
  BERDecodeCallback = function(ber: PBerElement; data: Pointer; mode: cint): cint; cdecl;


function ber_get_tag(ber: PBerElement): ber_tag_t; cdecl; external;

function ber_skip_tag(ber: PBerElement; var len: ber_len_t): ber_tag_t; cdecl; external;

function ber_peek_tag(ber: PBerElement; var len: ber_len_t): ber_tag_t; cdecl; external;

function ber_get_int(ber: PBerElement; var num: ber_int_t): ber_tag_t; cdecl; external;

function ber_get_enum(ber: PBerElement; var num: ber_int_t): ber_tag_t; cdecl; external;

function ber_get_stringb(ber: PBerElement; buf: pcchar; var len: ber_len_t): ber_tag_t; cdecl; external;

function ber_get_stringbv(ber: PBerElement; bv: PBerval; alloc: cbool): ber_tag_t; cdecl; external;

function ber_get_stringa(ber: PBerElement; var buf: pcchar): ber_tag_t; cdecl; external;

function ber_get_stringal(ber: PBerElement; var bv: PBerval): ber_tag_t; cdecl; external;

function ber_get_bitstringa(ber: PBerElement; var buf: pcchar; var len: ber_len_t): ber_tag_t; cdecl; external;

function ber_get_null(ber: PBerElement): ber_tag_t; cdecl; external;

function ber_get_Boolean(ber: PBerElement; var boolval: ber_int_t): ber_tag_t; cdecl; external;

function ber_first_element(ber: PBerElement; var len: ber_len_t; var last: pcchar): ber_tag_t; cdecl; external;

function ber_next_element(ber: PBerElement; var len: ber_len_t; const last: pcchar): ber_tag_t; cdecl; external;

function ber_scanf(ber: PBerElement;const fmt: pcchar): ber_tag_t; cdecl; varargs; external;


(*
 * in encode.c
 *)
type
  BEREncodeCallback = function(ber: PBerElement; data: Pointer): cint;


function ber_put_enum(ber: PBerElement; num: ber_int_t; tag: ber_tag_t): cint; cdecl; external;

function ber_put_int(ber: PBerElement; num: ber_int_t; tag: ber_tag_t): cint; cdecl; external;

function ber_put_ostring(ber: PBerElement; const str: pcchar; len: ber_len_t; tag: ber_tag_t): cint; cdecl; external;

function ber_put_berval(ber: PBerElement; bv: PBerval; tag: ber_tag_t): cint; cdecl; external;

function ber_put_string(ber: PBerElement; const str: pcchar; tag: ber_tag_t): cint; cdecl; external;

function ber_put_bitstring(ber: PBerElement; const str: pcchar; bitlen: ber_len_t; tag: ber_tag_t): cint; cdecl; external;

function ber_put_null(ber: PBerElement; tag: ber_tag_t): cint; cdecl; external;

function ber_put_boolean(ber: PBerElement; boolval: ber_int_t; tag: ber_tag_t): cint; cdecl; external;

function ber_start_seq(ber: PBerElement; tag: ber_tag_t): cint; cdecl; external;

function ber_start_set(ber: PBerElement; tag: ber_tag_t): cint; cdecl; external;

function ber_put_seq(ber: PBerElement): cint; cdecl; external;

function ber_put_set(ber: PBerElement): cint; cdecl; external;

function ber_printf(ber: PBerElement; const fmt: pcchar): cint; cdecl; varargs; external;


(*
 * in io.c:
 *)

function ber_read(ber: PBerElement; buf: pcchar; len: ber_len_t): ber_slen_t; cdecl; external;

function ber_write(ber: PBerElement; const buf: pcchar; len: ber_len_t; nosos: cint): ber_slen_t; cdecl; external;

procedure ber_free(ber: PBerElement; freebuf: cbool); cdecl; external;

procedure ber_free_buf(ber: PBerElement); cdecl; external;

function ber_flush(sb: PSockbuf; ber: PBerElement; freeit: cbool): cint; cdecl; external;

function ber_alloc: PBerElement; cdecl; external; deprecated;

function der_alloc: PBerElement; cdecl; external; deprecated;

function ber_alloc_t(beroptions: cint): PBerElement; cdecl; external;

function ber_dup(ber: PBerElement): PBerElement; cdecl; external;

function ber_get_next(sb: PSockbuf; var len: ber_len_t; ber: PBerElement): ber_tag_t; cdecl; external;

procedure ber_init2(ber: PBerElement; bv: PBerval; options: cint); cdecl; external;

procedure ber_init_w_nullc(ber: PBerElement; options: cint); cdecl; external; deprecated;

procedure ber_reset(ber: PBerElement; was_writing: cbool); cdecl; external;

function ber_init(bv: PBerval): PBerElement; cdecl; external;

function ber_flatten(ber: PBerElement; var bvPtr: PBerval): cint; cdecl; external;

function ber_flatten2(ber: PBerElement; bv: PBerval; alloc: cbool): cint; cdecl; external;

function ber_remaining(ber: PBerElement): cint; cdecl; external;


(*
 * LBER ber accessor functions
 *)

function ber_get_option(item: Pointer; option: cint; outvalue: Pointer): cint; cdecl; external;

function ber_set_option(item: Pointer; option: cint; const invalue: Pointer): cint; cdecl; external;


(*
 * LBER sockbuf.c
 *)

function ber_sockbuf_alloc: PSockbuf; cdecl; external;

procedure ber_sockbuf_free(sb: PSockbuf); cdecl; external;

function ber_sockbuf_add_io(sb: PSockbuf; sbio: PSockbuf_IO; layer: cint; arg: Pointer): cint; cdecl; external;

function ber_sockbuf_remove_io(sb: PSockbuf; sbio: PSockbuf_IO; layer: cint): cint; cdecl; external;

function ber_sockbuf_ctrl(sb: PSockbuf; opt: cint; arg: Pointer): cint; cdecl; external;

var
  ber_sockbuf_io_tcp                : Sockbuf_IO; cvar; external;
  ber_sockbuf_io_readahead          : Sockbuf_IO; cvar; external;
  ber_sockbuf_io_fd                 : Sockbuf_IO; cvar; external;
  ber_sockbuf_io_debug              : Sockbuf_IO; cvar; external;
{$IFDEF LDAP_CONNECTIONLESS}
  ber_sockbuf_io_udp                : Sockbuf_IO; cvar; external;
{$ENDIF}


(*
 * LBER memory.c
 *)

function ber_memalloc(s: ber_len_t): Pointer; cdecl; external;

function ber_memrealloc(p: Pointer; s: ber_len_t): Pointer; cdecl; external;

function ber_memcalloc(n: ber_len_t; s: ber_len_t): Pointer; cdecl; external;

procedure ber_memfree(p: Pointer); cdecl; external;

procedure ber_memvfree(vector: PPointer); cdecl; external;

procedure ber_bvfree(bv: PBerval); cdecl; external;

procedure ber_bvecfree(bv: PPBerval); cdecl; external;

function ber_bvecadd(var bvec: PPBerval; bv: PBerval): cint; cdecl; external;

function ber_dupbv(dst: PBerval; src: PBerval): PBerval; cdecl; external;

function ber_bvdup(src: PBerval): PBerval; cdecl; external;

function ber_str2bv(const str: pcchar; len: ber_len_t; duplicate: cbool; bv: PBerval): PBerval; cdecl; external;

function ber_mem2bv(const str: pcchar; len: ber_len_t; duplicate: cbool; bv: PBerval): PBerval; cdecl; external;

function ber_bvstr(const str: pcchar): PBerval;

function ber_bvstrdup(const str: pcchar): PBerval;

function ber_strdup(const str: pcchar): pcchar; cdecl; external;

procedure ber_bvarray_free(p: BerVarray); cdecl; external;

function ber_bvarray_add(p: PBerVarray; bv: PBerval): cint; cdecl; external;

function ber_bvcmp(v1, v2: PBerval): cint;


(*
 * error.c
 *)

function ber_errno_addr: pcint; cdecl; external;

function ber_errno: cint;

const
  LBER_ERROR_NONE                   = $0;
  LBER_ERROR_PARAM                  = $1;
  LBER_ERROR_MEMORY                 = $2;
