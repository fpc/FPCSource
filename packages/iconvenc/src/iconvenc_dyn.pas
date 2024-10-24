{
    This file is part of the Free Pascal run time library.
    Copyright (c) 2000 by Marco van de Voort(marco@freepascal.org)
    member of the Free Pascal development team

    libiconv header translation + a helper routine  
    http://wiki.freepascal.org/iconvenc Dynamic version

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright. (LGPL)

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

}
{$IFNDEF FPC_DOTTEDUNITS}
unit iconvenc_dyn;
{$ENDIF FPC_DOTTEDUNITS}

interface
{$mode objfpc}{$H+}

{$IFDEF FPC_DOTTEDUNITS}
uses
  System.CTypes,UnixApi.Types,UnixApi.Base,
  UnixApi.Dl,
  System.InitC;
{$ELSE FPC_DOTTEDUNITS}
uses
  ctypes,unixtype,baseunix,
  dl,
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

var
  iconv_lib: pointer;
  iconv_open: Ticonv_open;
  iconv: Ticonv;
  iconv_close: Ticonv_close;
  IconvLibFound: boolean = False;

function TryLoadLib(LibName: string; var error: String): boolean; // can be used to load non standard libname
function Iconvert(s: AnsiString; var res: AnsiString; const FromEncoding, ToEncoding: AnsiString): cint;
function InitIconv(var error: AnsiString): boolean;

implementation

function TryLoadLib(LibName: string; var error: String): boolean;

    function resolvesymbol (var funcptr; symbol: string): Boolean;
    begin
      pointer(funcptr) := pointer(dlsym(iconv_lib, PAnsiChar(symbol)));
      result := assigned(pointer(funcptr));
      if not result then
        error := error+#13#10+dlerror();
    end;

var
  res: boolean;
begin
  result := false;
  Error := Error+#13#10'Trying '+LibName;
  iconv_lib := dlopen(PAnsiChar(libname), RTLD_NOW);
  if Assigned(iconv_lib) then
  begin
    result := true;
    result := result and resolvesymbol(pointer(iconv),'iconv');
    result := result and resolvesymbol(pointer(iconv_open),'iconv_open');
    result := result and resolvesymbol(pointer(iconv_close),'iconv_close');
    if not result then
      begin
        result:=true;
        result := result and resolvesymbol(pointer(iconv),'libiconv');
        result := result and resolvesymbol(pointer(iconv_open),'libiconv_open');
        result := result and resolvesymbol(pointer(iconv_close),'libiconv_close');
      end;
//    if not res then
//      dlclose(iconv_lib);
  end else
    error:=error+#13#10+dlerror();
end;


function InitIconv(var error: string): boolean;
begin
  result := true;
  error := '';
  if not TryLoadLib('libc.so.6', error) then
    if not TryLoadLib('libiconv.so', error) then
    {$if defined(haiku)}
        if not TryLoadLib('libtextencoding.so', error) then
      {$ifend}
      result := false; 
  iconvlibfound := iconvlibfound or result;
end;

{$i iconvert.inc}

end.
