{
    $Id$
    This file is part of the Free Pascal run time library.
    Copyright (c) 2005 by Florian Klaempfl,
    member of the Free Pascal development team.

    libc based wide string support

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
 **********************************************************************}

{ *********************************************************************** }
{  Parts are Copyright (c) 2005 Andreas Hausladen                         }
{                                                                         }
{  This software is provided 'as-is', without any express or              }
{  implied warranty. In no event will the author be held liable           }
{  for any damages arising from the use of this software.                 }
{                                                                         }
{  Permission is granted to anyone to use this software for any           }
{  purpose, including commercial applications, and to alter it            }
{  and redistribute it freely, subject to the following                   }
{  restrictions:                                                          }
{                                                                         }
{    1. The origin of this software must not be misrepresented,           }
{       you must not claim that you wrote the original software.          }
{       If you use this software in a product, an acknowledgment          }
{       in the product documentation would be appreciated but is          }
{       not required.                                                     }
{                                                                         }
{    2. Altered source versions must be plainly marked as such, and       }
{       must not be misrepresented as being the original software.        }
{                                                                         }
{    3. This notice may not be removed or altered from any source         }
{       distribution.                                                     }
{                                                                         }
{ *********************************************************************** }

{$mode objfpc}

unit cwstring;

interface

 {$linklib c}

Procedure SetCWidestringManager;

implementation

{$linklib c}

Uses
  BaseUnix,
  ctypes,
  unix,
  unixtype,
  sysutils,
  initc;


{ Case-mapping "arrays" }
var
  AnsiUpperChars: AnsiString; // 1..255
  AnsiLowerChars: AnsiString; // 1..255
  WideUpperChars: WideString; // 1..65535
  WideLowerChars: WideString; // 1..65535


{ the following declarations are from the libc unit for linux so they
  might be very linux centric
  maybe this needs to be splitted in an os depend way later }
function towlower(__wc:wint_t):wint_t;cdecl;external;
function towupper(__wc:wint_t):wint_t;cdecl;external;
function wcscoll(__s1:pwchar_t; __s2:pwchar_t):longint;cdecl;external;

const
  __LC_CTYPE = 0;
  _NL_CTYPE_CLASS = (__LC_CTYPE shl 16);
  _NL_CTYPE_CODESET_NAME = (_NL_CTYPE_CLASS)+14;
  CODESET = _NL_CTYPE_CODESET_NAME;

{ unicode encoding name }
{$ifdef FPC_LITTLE_ENDIAN}
  unicode_encoding = 'UNICODELITTLE';
{$else  FPC_LITTLE_ENDIAN}
  unicode_encoding = 'UNICODEBIG';
{$endif  FPC_LITTLE_ENDIAN}

type
  piconv_t = ^iconv_t;
  iconv_t = pointer;
  nl_item = longint;

function nl_langinfo(__item:nl_item):pchar;cdecl;external;
function iconv_open(__tocode:pchar; __fromcode:pchar):iconv_t;cdecl;external;
function iconv(__cd:iconv_t; __inbuf:ppchar; __inbytesleft:psize_t; __outbuf:ppchar; __outbytesleft:psize_t):size_t;cdecl;external;
function iconv_close(__cd:iconv_t):longint;cdecl;external;

var
  iconv_ansi2wide,
  iconv_wide2ansi : iconv_t;

procedure Wide2AnsiMove(source:pwidechar;var dest:ansistring;len:SizeInt);
  var
    outlength,
    outoffset,
    outleft : size_t;
    srcpos : pwidechar;
    destpos: pchar;
    mynil : pchar;
    my0 : size_t;
  begin
    mynil:=nil;
    my0:=0;
    { rought estimation }
    setlength(dest,len*3);
    outlength:=len*3;
    srcpos:=source;
    destpos:=pchar(dest);
    outleft:=outlength;
    while iconv(iconv_wide2ansi,@srcpos,@len,@destpos,@outleft)=size_t(-1) do
      begin
        case fpgetCerrno of
          ESysEINVAL:
            { sometimes it seems to be necessary to reset the conversion context }
            iconv(iconv_wide2ansi,@mynil,@my0,@mynil,@my0);
          ESysE2BIG:
            begin
              outoffset:=destpos-pchar(dest);
              { extend }
              setlength(dest,outlength+len*3);
              inc(outleft,len*3);
              inc(outlength,len*3);
              { string could have been moved }
              destpos:=pchar(dest)+outoffset;
            end;
          else
            raise EConvertError.Create('iconv error');
        end;
      end;
    // truncate string
    setlength(dest,length(dest)-outleft);
  end;


procedure Ansi2WideMove(source:pchar;var dest:widestring;len:SizeInt);
  var
    outlength,
    outoffset,
    outleft : size_t;
    srcpos,
    destpos: pchar;
    mynil : pchar;
    my0 : size_t;
  begin
    mynil:=nil;
    my0:=0;
    setlength(dest,len);
    outlength:=len;
    srcpos:=source;
    destpos:=pchar(dest);
    outleft:=outlength*2;
    while iconv(iconv_ansi2wide,@srcpos,@len,@destpos,@outleft)=size_t(-1) do
      begin
        case fpgetCerrno of
          ESysEINVAL:
            { sometimes it seems to be necessary to reset the conversion context }
            iconv(iconv_wide2ansi,@mynil,@my0,@mynil,@my0);
          ESysE2BIG:
            begin
              outoffset:=destpos-pchar(dest);
              { extend }
              setlength(dest,outlength+len);
              inc(outleft,len*2);
              inc(outlength,len);
              { string could have been moved }
              destpos:=pchar(dest)+outoffset;
            end;
          else
            raise EConvertError.Create('iconv error');
        end;
      end;
    // truncate string
    setlength(dest,length(dest)-outleft div 2);
  end;


function LowerWideString(const s : WideString) : WideString;
  var
    i : SizeInt;
  begin
    SetLength(result,length(s));
    for i:=1 to length(s) do
      result[i]:=WideChar(towlower(wint_t(s[i])));
  end;


function UpperWideString(const s : WideString) : WideString;
  var
    i : SizeInt;
  begin
    SetLength(result,length(s));
    for i:=1 to length(s) do
      result[i]:=WideChar(towupper(wint_t(s[i])));
  end;


function CompareWideString(const s1, s2 : WideString) : PtrInt;
  begin
  end;


function CompareTextWideString(const s1, s2 : WideString): PtrInt;
  begin
  end;

Var
  CWideStringManager : TWideStringManager;

Procedure SetCWideStringManager;

begin
  With CWideStringManager do
    begin
      Wide2AnsiMoveProc:=@Wide2AnsiMove;
      Ansi2WideMoveProc:=@Ansi2WideMove;

      UpperWideStringProc:=@UpperWideString;
      LowerWideStringProc:=@LowerWideString;
      {
      CompareWideStringProc
      CompareTextWideStringProc
      CharLengthPCharProc

      UpperAnsiStringProc
      LowerAnsiStringProc
      CompareStrAnsiStringProc
      CompareTextAnsiStringProc
      StrCompAnsiStringProc
      StrICompAnsiStringProc
      StrLCompAnsiStringProc
      StrLICompAnsiStringProc
      StrLowerAnsiStringProc
      StrUpperAnsiStringProc
      }
    end;
  SetWideStringManager(CWideStringManager);
end;


procedure InitCharArrays;
  var
    i: longint;
  begin
{$ifdef dummy}
    // first initialize the WideChar arrays
    SetLength(WideUpperChars, +1);
    SetLength(WideLowerChars, High(WideChar)+1);
    for i := 0 to High(WideChar) do
      WideUpperChars[i+1]:=WideChar(towupper(wint_t(i)));
    for i := 0 to High(WideChar) do
      WideLowerChars[i+1]:=WideChar(towlower(wint_t(i)));

    // use the widechar array to initialize the AnsiChar arrays
    SetLength(AnsiUpperChars, Byte(High(Char)) + 1);
    SetLength(AnsiLowerChars, Byte(High(Char)) + 1);
    for i:=0 to High(Char) do
      AnsiUpperChars[i+1]:=AnsiChar(i);
    for i:=0 to High(Char) do
      AnsiLowerChars[i+1]:=AnsiChar(i);
    AnsiUpperChars:=WideUpperCase(AnsiUpperChars);
    AnsiLowerChars:=WideLowerCase(AnsiLowerChars);
{$endif}
  end;

initialization
  SetCWideStringManager;
  InitCharArrays;
  { init conversion tables }
  writeln(nl_langinfo(CODESET));
  iconv_ansi2wide:=iconv_open(nl_langinfo(CODESET),unicode_encoding);
  iconv_wide2ansi:=iconv_open(unicode_encoding,nl_langinfo(CODESET));
finalization
  iconv_close(iconv_ansi2wide);
end.

{
  $Log$
  Revision 1.4  2005-03-16 22:26:12  florian
    + ansi<->wide implemented using iconv

  Revision 1.3  2005/02/14 17:13:31  peter
    * truncate log

  Revision 1.2  2005/02/03 18:40:57  florian
    + infrastructure for WideCompareText implemented

  Revision 1.1  2005/02/01 20:22:50  florian
    * improved widestring infrastructure manager
}
