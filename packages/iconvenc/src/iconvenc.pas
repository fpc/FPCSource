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
unit iconvenc;

interface
{$mode objfpc}{$H+}

{DEFINE LOADDYNAMIC}

uses
  baseunix,
  {$ifdef LOADDYNAMIC}
  dl,
  {$endif}
  initc;

const
  n = 1;

type
   piconv_t = ^iconv_t;
   iconv_t = pointer;

   Ticonv_open = function(__tocode: pchar; __fromcode: pchar): iconv_t; cdecl;
   Ticonv = function(__cd: iconv_t; __inbuf: ppchar; __inbytesleft: psize_t; __outbuf: ppchar; __outbytesleft: psize_t): size_t; cdecl;
   Ticonv_close = function(__cd: iconv_t): cint; cdecl;

{$IFNDEF LOADDYNAMIC}
{$ifndef Linux}    // and other OSes with iconv in libc.
{$linklib iconv}
{$endif}
function iconv_open(__tocode: pchar; __fromcode: pchar): iconv_t; cdecl; external;
function iconv (__cd: iconv_t; __inbuf: ppchar; __inbytesleft: psize_t; __outbuf: ppchar; __outbytesleft: psize_t): size_t; cdecl; external;
function iconv_close (__cd: iconv_t): cint; cdecl; external;

var
  IconvLibFound: boolean = False;

{$ELSE}
var
  iconv_lib: pointer;
  iconv_open: Ticonv_open;
  iconv: Ticonv;
  iconv_close: Ticonv_close;
  IconvLibFound: boolean = true;

function TryLoadLib(LibName: string; var error: string): boolean; // can be used to load non standard libname
{$endif}

function Iconvert(s: string; var res: string; FromEncoding, ToEncoding: string): cint;
function InitIconv(var error: string): boolean;

implementation

{$IFDEF LOADDYNAMIC}
function TryLoadLib(LibName: string; var error: string): boolean;

    function resolvesymbol (var funcptr; symbol: string): Boolean;
    begin
      pointer(funcptr) := pointer(dlsym(iconv_lib, pchar(symbol)));
      result := assigned(pointer(funcptr));
      if not result then
        error := error+#13#10+dlerror();
    end;

var
  res: boolean;
begin
  result := false;
  Error := Error+#13#10'Trying '+LibName;
  iconv_lib := dlopen(pchar(libname), RTLD_NOW);
  if Assigned(iconv_lib) then
  begin
    result := true;
    result := result and resolvesymbol(pointer(iconv),'iconv');
    result := result and resolvesymbol(pointer(iconv_open),'iconv_open');
    result := result and resolvesymbol(pointer(iconv_close),'iconv_close');
//    if not res then
//      dlclose(iconv_lib);
  end else
    error:=error+#13#10+dlerror();
end;

{$ENDIF}

function InitIconv(var error: string): boolean;
begin
  result := true;
  {$ifdef LOADDYNAMIC}
  error := '';
  if not TryLoadLib('libc.so.6', error) then
    if not TryLoadLib('libiconv.so', error) then
      result := false; 
  {$endif}
  iconvlibfound := iconvlibfound or result;
end;

function Iconvert(S: string; var Res: string; FromEncoding, ToEncoding: string): cint;
var
  InLen, OutLen, Offset: size_t;
  Src, Dst: pchar;
  H: iconv_t;
  lerr: cint;
  iconvres: size_t;
begin
  H := iconv_open(PChar(ToEncoding), PChar(FromEncoding));
  if not assigned(H) then
  begin
    Res := S;
    exit(-1);
  end;

  try
    SetLength(Res, Length(S));
    InLen := Length(S);
    OutLen := Length(Res);
    Src := PChar(S);
    Dst := PChar(Res);

    while InLen > 0 do
    begin
      iconvres := iconv(H, @Src, @InLen, @Dst, @OutLen);
      if iconvres = size_t(-1) then
      begin
        lerr := cerrno;
        if lerr = ESysEILSEQ then // unknown char, skip
          begin
            Dst^ := Src^;
            Inc(Src);
            Inc(Dst);
            Dec(InLen);
            Dec(OutLen);
          end
        else
          if lerr = ESysE2BIG then
            begin
              Offset := Dst - PChar(Res);
              SetLength(Res, Length(Res)+InLen*2+5); // 5 is minimally one utf-8 char
              Dst := PChar(Res) + Offset;
              OutLen := Length(Res) - Offset;
            end
          else
            exit(-1)
      end;
    end;

    // iconv has a buffer that needs flushing, specially if the last char is not #0
    iconv(H, nil, nil, @Dst, @Outlen);

    // trim output buffer
    SetLength(Res, Length(Res) - Outlen);
  finally
    iconv_close(H);
  end;

  Result := 0;
end;

end.

