{
    This file is part of the Free Pascal run time library.
    Copyright (c) 2011 by Jonas Maebe,
    member of the Free Pascal development team.

    This file implements support routines for typed constants for the JVM
    platform

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}

procedure fpc_tcon_shortint_array_from_string(const s: unicodestring; var arr: array of shortint; startindex, len: longint); compilerproc;
procedure fpc_tcon_byte_array_from_string(const s: unicodestring; var arr: array of byte; startindex, len: longint); compilerproc;

procedure fpc_tcon_smallint_array_from_string(const s: unicodestring; var arr: array of smallint; startindex, len: longint); compilerproc;
procedure fpc_tcon_word_array_from_string(const s: unicodestring; var arr: array of word; startindex, len: longint); compilerproc;

procedure fpc_tcon_longint_array_from_string(const s: unicodestring; var arr: array of longint; startindex, len: longint); compilerproc;
procedure fpc_tcon_cardinal_array_from_string(const s: unicodestring; var arr: array of cardinal; startindex, len: longint); compilerproc;

procedure fpc_tcon_int64_array_from_string(const s: unicodestring; var arr: array of int64; startindex, len: longint); compilerproc;
procedure fpc_tcon_qword_array_from_string(const s: unicodestring; var arr: array of qword; startindex, len: longint); compilerproc;

