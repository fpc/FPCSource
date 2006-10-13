{ Licensed to the Apache Software Foundation (ASF) under one or more
 * contributor license agreements.  See the NOTICE file distributed with
 * this work for additional information regarding copyright ownership.
 * The ASF licenses this file to You under the Apache License, Version 2.0
 * (the "License"); you may not use this file except in compliance with
 * the License.  You may obtain a copy of the License at
 *
 *     http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 }

{$ifdef B_SFIO}
#include "sfio.h"
{$endif}

//#include <stdarg.h>

const
{ Reading is buffered }
  B_RD    = (1);
{ Writing is buffered }
  B_WR    = (2);
  B_RDWR  = (3);
{ At end of file, or closed stream; no further input allowed }
  B_EOF   = (4);
{ No further output possible }
  B_EOUT  = (8);
{ A read error has occurred }
  B_RDERR =(16);
{ A write error has occurred }
  B_WRERR =(32);
//#ifdef B_ERROR  { in SVR4: sometimes defined in /usr/include/sys/buf.h }
//#undef B_ERROR
//#endif
  B_ERROR =(48);
{ Use chunked writing }
  B_CHUNK =(64);
{ bflush() if a read would block }
  B_SAFEREAD =(128);
{ buffer is a socket }
  B_SOCKET =(256);
{$ifdef CHARSET_EBCDIC}
  B_ASCII2EBCDIC = $40000000;  { Enable conversion for this buffer }
  B_EBCDIC2ASCII = $80000000;  { Enable conversion for this buffer }
{$endif} {CHARSET_EBCDIC}

type
  Pbuff_struct = ^buff_struct;

  PBUFF = Pbuff_struct;

  PPBUFF = ^PBUFF;

  error_t = procedure (fb: PBUFF; op: cint; data: Pointer);
  
  filter_callback_t = procedure (param1: PBUFF; const param2: Pointer; param3: cint);

  buff_struct = record
    flags: cint;		{ flags }
    inptr: PChar;      	        { pointer to next location to read }
    incnt: cint;		{ number of bytes left to read from input buffer;
				 * always 0 if had a read error  }
    outchunk: cint;		{ location of chunk header when chunking }
    outcnt: cint;		{ number of byte put in output buffer }
    inbase: PChar;
    outbase: PChar;
    bufsiz: cint;
    error: error_t;
    error_data: PChar;
    bytes_sent: clong;  	{ number of bytes actually written }

    pool: Pap_pool;

{ could also put pointers to the basic I/O routines here }
    fd: cint;			{ the file descriptor }
    fd_in: cint;		{ input file descriptor, if different }
{$ifdef WINDOWS}
    hFH: HANDLE;		{ Windows filehandle }
{$endif}

    { transport handle, for RPC binding handle or some such }
    t_handle: PChar;

{$ifdef B_SFIO}
    sf_in: PSfio_t;
    sf_out: PSfio_t;
{$endif}

    callback_data: Pointer;
    filter_callback: filter_callback_t;
	
  end;

{$ifdef B_SFIO}
  apache_sfio = record
    disc: Sfdisc_t;
    buff: PBUFF;
  end;

extern Sfdisc_t *bsfio_new(pool *p, BUFF *b);
{$endif}

{ Options to bset/getopt }
const
  BO_BYTECT = (1);

{ Stream creation and modification }

function ap_bcreate(p: PPool; flags: cint): PBUFF;
 {$IFDEF WINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF} external LibHTTPD;

procedure ap_bpushfd(fb: PBUFF; fd_in, fd_out: cint);
 {$IFDEF WINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF} external LibHTTPD;

{$ifdef WINDOWS}

procedure ap_bpushh(fb: PBUFF; hFH: HANDLE);
 {$IFDEF WINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF} external LibHTTPD;

{$endif}

function ap_bsetopt(fb: PBUFF; optname: cint; const optval: Pointer): cint;
 {$IFDEF WINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF} external LibHTTPD;

function ap_bgetopt(fb: PBUFF; optname: cint; optval: Pointer): cint;
 {$IFDEF WINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF} external LibHTTPD;

function ap_bsetflag(fb: PBUFF; flag, value: cint): cint;
 {$IFDEF WINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF} external LibHTTPD;

function ap_bclose(fb: PBUFF): cint;
 {$IFDEF WINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF} external LibHTTPD;

//#define ap_bgetflag(fb, flag)	((fb)->flags & (flag))

{ Error handling }

procedure ap_bonerror(fb: PBUFF; error: error_t; data: Pointer);
 {$IFDEF WINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF} external LibHTTPD;

{ I/O }
function ap_bread(fb: PBUFF; buf: Pointer; nbyte: cint): cint;
 {$IFDEF WINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF} external LibHTTPD;
{API_EXPORT(int) ap_bgets(char *s, int n, BUFF *fb);
API_EXPORT(int) ap_blookc(char *buff, BUFF *fb);
API_EXPORT(int) ap_bskiplf(BUFF *fb);
API_EXPORT(int) ap_bwrite(BUFF *fb, const void *buf, int nbyte);
API_EXPORT(int) ap_bflush(BUFF *fb);
API_EXPORT(int) ap_bputs(const char *x, BUFF *fb);
API_EXPORT_NONSTD(int) ap_bvputs(BUFF *fb,...);
API_EXPORT_NONSTD(int) ap_bprintf(BUFF *fb, const char *fmt,...)
				__attribute__((format(printf,2,3)));
API_EXPORT(int) ap_vbprintf(BUFF *fb, const char *fmt, va_list vlist);
}
{ Internal routines }
{API_EXPORT(int) ap_bflsbuf(int c, BUFF *fb);
API_EXPORT(int) ap_bfilbuf(BUFF *fb);

#ifndef CHARSET_EBCDIC

#define ap_bgetc(fb)   ( ((fb)->incnt == 0) ? ap_bfilbuf(fb) : \
		    ((fb)->incnt--, *((fb)->inptr++)) )

#define ap_bputc(c, fb) ((((fb)->flags & (B_EOUT|B_WRERR|B_WR)) != B_WR || \
		     (fb)->outcnt == (fb)->bufsiz) ? ap_bflsbuf(c, (fb)) : \
		     ((fb)->outbase[(fb)->outcnt++] = (c), 0))

#else} {CHARSET_EBCDIC}
{
#define ap_bgetc(fb)   ( ((fb)->incnt == 0) ? ap_bfilbuf(fb) : \
		    ((fb)->incnt--, (fb->flags & B_ASCII2EBCDIC)\
		    ?os_toebcdic[(unsigned char)*((fb)->inptr++)]:*((fb)->inptr++)) )

#define ap_bputc(c, fb) ((((fb)->flags & (B_EOUT|B_WRERR|B_WR)) != B_WR || \
		     (fb)->outcnt == (fb)->bufsiz) ? ap_bflsbuf(c, (fb)) : \
		     ((fb)->outbase[(fb)->outcnt++] = (fb->flags & B_EBCDIC2ASCII)\
		     ?os_toascii[(unsigned char)c]:(c), 0))

#endif} {CHARSET_EBCDIC}

type
  child_info = record
{$ifdef WINDOWS}
    {
     *  These handles are used by ap_call_exec to call 
     *  create process with pipe handles.
     }
    hPipeInputRead: HANDLE;
    hPipeOutputWrite: HANDLE;
    hPipeErrorWrite: HANDLE;
{$else}
    {
     * We need to put a dummy member in here to avoid compilation
     * errors under certain Unix compilers, like SGI's and HPUX's,
     * which fail to compile a zero-sized struct.  Of course
     * it would be much nicer if there was actually a use for this
     * structure under Unix.  Aah the joys of x-platform code.
     }
    dummy: cint;
{$endif}
  end;
  
  Pchild_info = ^child_info;
  
  func_t = function (param1: Pointer; param2: Pchild_info): cint;

function ap_bspawn_child(p: PPool;
 func: func_t; data: Pointer; kill_how: kill_conditions; pipe_in, pipe_out, pipe_err: PPBUFF): cint;
 {$IFDEF WINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF} external LibHTTPD;

{ enable non-blocking operations }
function ap_bnonblock(fb: PBUFF; direction: cint): cint;
 {$IFDEF WINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF} external LibHTTPD;
{ and get an fd to select() on }
function ap_bfileno(fb: PBUFF; direction: cint): cint;
 {$IFDEF WINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF} external LibHTTPD;

{ bflush() if a read now would block, but don't actually read anything }
procedure ap_bhalfduplex(fb: PBUFF);
 {$IFDEF WINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF} external LibHTTPD;

{$if defined(WINDOWS) or defined(NETWARE) or defined(CYGWIN_WINSOCK)}

{ ap_recvwithtimeout/ap_sendwithtimeout socket primitives for WinSock }
//API_EXPORT(int) ap_sendwithtimeout(int sock, const char *buf, int len, int flags);
//API_EXPORT(int) ap_recvwithtimeout(int sock, char *buf, int len, int flags);

{$endif}

