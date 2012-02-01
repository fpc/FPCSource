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
  var
    i: longint;
    c: widechar;
  begin
    for i:=0 to (len div 2)-1 do
      begin
        c:=s[i+1];
        arr[startindex+i*2]:=shortint(ord(c) shr 8);
        arr[startindex+i*2+1]:=shortint(ord(c));
      end;
    if odd(len) then
      arr[startindex+len-1]:=ord(s[len div 2 + 1]) shr 8;
  end;


procedure fpc_tcon_smallint_array_from_string(const s: unicodestring; var arr: array of smallint; startindex, len: longint); compilerproc;
  var
    i: longint;
  begin
    for i:=0 to len-1 do
      arr[startindex+i]:=ord(s[i+1]);
  end;


procedure fpc_tcon_longint_array_from_string(const s: unicodestring; var arr: array of longint; startindex, len: longint); compilerproc;
  var
    i, l: longint;
  begin
    { TODO: maybe optimize encoding via uleb/sleb }
    for i:=0 to len-1 do
      begin
        l:=ord(s[i*2+1]) shl 16;
        l:=l or ord(s[i*2+2]);
        arr[startindex+i]:=l;
      end;
  end;


procedure fpc_tcon_int64_array_from_string(const s: unicodestring; var arr: array of int64; startindex, len: longint); compilerproc;
  var
    i: longint;
    l: int64;
  begin
    { TODO: maybe optimize encoding via uleb/sleb }
    for i:=0 to len-1 do
      begin
        l:=int64(ord(s[i*4+1])) shl 48;
        l:=l or (int64(ord(s[i*4+2])) shl 32);
        l:=l or (ord(s[i*4+3]) shl 16);
        l:=l or ord(s[i*4+4]);
        arr[startindex+i]:=l;
      end;
  end;

{ specifying compilerprocs using an external name doesn't work yet }

procedure fpc_tcon_shortint_array_from_string_intern_as_byte(const s: unicodestring; var arr: array of byte; startindex, len: longint); external name 'fpc_tcon_shortint_array_from_string';
procedure fpc_tcon_smallint_array_from_string_intern_as_word(const s: unicodestring; var arr: array of word; startindex, len: longint); external name 'fpc_tcon_smallint_array_from_string';
procedure fpc_tcon_longint_array_from_string_intern_as_cardinal(const s: unicodestring; var arr: array of cardinal; startindex, len: longint); external name 'fpc_tcon_longint_array_from_string';
procedure fpc_tcon_int64_array_from_string_intern_as_int64(const s: unicodestring; var arr: array of qword; startindex, len: longint); external name 'fpc_tcon_int64_array_from_string';


procedure fpc_tcon_byte_array_from_string(const s: unicodestring; var arr: array of byte; startindex, len: longint); compilerproc;
  begin
    fpc_tcon_shortint_array_from_string_intern_as_byte(s,arr,startindex,len);
  end;


procedure fpc_tcon_word_array_from_string(const s: unicodestring; var arr: array of word; startindex, len: longint); compilerproc;
  begin
    fpc_tcon_smallint_array_from_string_intern_as_word(s,arr,startindex,len);
  end;


procedure fpc_tcon_cardinal_array_from_string(const s: unicodestring; var arr: array of cardinal; startindex, len: longint); compilerproc;
  begin
    fpc_tcon_longint_array_from_string_intern_as_cardinal(s,arr,startindex,len);
  end;


procedure fpc_tcon_qword_array_from_string(const s: unicodestring; var arr: array of qword; startindex, len: longint); compilerproc;
  begin
    fpc_tcon_int64_array_from_string_intern_as_int64(s,arr,startindex,len);
  end;


