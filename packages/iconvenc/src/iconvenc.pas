{
    This file is part of the Free Pascal run time library.
    Copyright (c) 2000 by Marco van de Voort(marco@freepascal.org)
    member of the Free Pascal development team

    libiconv header translation + a helper routine  
    http://wiki.freepascal.org/iconvenc

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright. (LGPL)

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

}
{$IFNDEF FPC_DOTTEDUNITS}
unit iconvenc;
{$ENDIF FPC_DOTTEDUNITS}

interface
{$mode objfpc}{$H+}


{$IFDEF FPC_DOTTEDUNITS}
uses
  System.CTypes,UnixApi.Types,UnixApi.Base,
  System.InitC;
{$ELSE FPC_DOTTEDUNITS}
uses
  ctypes,unixtype,baseunix,
  initc;
{$ENDIF FPC_DOTTEDUNITS}

const
  n = 1;

{$ifdef beos}
  ESysEILSEQ = EILSEQ;
{$endif}

type
   piconv_t = ^iconv_t;
   iconv_t = pointer;

   Ticonv_open = function(__tocode: PAnsiChar; __fromcode: PAnsiChar): iconv_t; cdecl;
   Ticonv = function(__cd: iconv_t; __inbuf: PPAnsiChar; __inbytesleft: psize_t; __outbuf: PPAnsiChar; __outbytesleft: psize_t): size_t; cdecl;
   Ticonv_close = function(__cd: iconv_t): cint; cdecl;

{$if not defined(linux) and not defined(solaris)}  // Linux (and maybe glibc pl
   {$if defined(haiku)}
    {$linklib textencoding}
   {$else}
    {$linklib iconv}
   {$endif}
 {$define useiconv}
{$endif linux}
          
Const
{$ifndef useiconv}
  libiconvname='c';  // is in libc under Linux.
{$else}
  {$ifdef haiku}
    libiconvname='textencoding';  // is in libtextencoding under Haiku
  {$else}
    libiconvname='iconv';
  {$endif}
{$endif}

{$if (defined(darwin) and defined(cpupowerpc32)) or defined(haiku)}
  iconvprefix='lib';  
{$else}
  iconvprefix='';
{$endif}
                           
function iconv_open(__tocode: PAnsiChar; __fromcode: PAnsiChar): iconv_t; cdecl; external libiconvname name iconvprefix+'iconv_open';
function iconv (__cd: iconv_t; __inbuf: PPAnsiChar; __inbytesleft: psize_t; __outbuf: PPAnsiChar; __outbytesleft: psize_t): size_t; cdecl; external libiconvname name iconvprefix+'iconv';
function iconv_close (__cd: iconv_t): cint; cdecl; external libiconvname name iconvprefix+'iconv_close';

var
  IconvLibFound: boolean = False;

function Iconvert(s: AnsiString; var res: AnsiString; const  FromEncoding, ToEncoding: AnsiString): cint;
function InitIconv(var error: String): boolean;

implementation

function InitIconv(var error:String ): boolean;
begin
  result := true;
  iconvlibfound := iconvlibfound or result;
end;

{$i iconvert.inc}

end.
