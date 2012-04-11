{
    This file is part of the Free Pascal run time library.
    Copyright (c) 2012 by Jonas Maebe

    Helper routines for cwstring AIX

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}

function Ansi2AnsiMove(source:pchar; fromcp:TSystemCodePage; const tocp: pchar; out dest:rawbytestring; len:SizeInt): boolean;
  var
    outlength,
    outoffset,
    outleft : size_t;
    use_iconv: iconv_t;
    srcpos,
    destpos: pchar;
    mynil : pchar;
    my0 : size_t;
    err: cint;
  begin
    use_iconv:=open_iconv_for_cps(fromcp,tocp,true);
    { unsupported encoding -> default move }
    if use_iconv=iconv_t(-1) then
      exit(false);
    mynil:=nil;
    my0:=0;
    // extra space
    outlength:=len;
    setlength(dest,outlength);
    srcpos:=source;
    destpos:=pchar(dest);
    outleft:=outlength;
    while iconv(use_iconv,@srcpos,psize(@len),@destpos,@outleft)=size_t(-1) do
      begin
        err:=fpgetCerrno;
        case err of
         ESysEINVAL,
         ESysEILSEQ:
            begin
              { skip and set to '?' }
              inc(srcpos);
              dec(len);
              pchar(destpos)^:='?';
              inc(destpos,2);
              dec(outleft,2);
              { reset }
              iconv(use_iconv,@mynil,@my0,@mynil,@my0);
              if err=ESysEINVAL then
                break;
            end;
          ESysE2BIG:
            begin
              outoffset:=destpos-pchar(dest);
              { extend }
              setlength(dest,outlength+len);
              inc(outleft,len);
              inc(outlength,len);
              { string could have been moved }
              destpos:=pchar(dest)+outoffset;
            end;
          else
            runerror(231);
        end;
      end;
    // truncate string
    setlength(dest,length(dest)-outleft);
    iconv_close(use_iconv);
    result:=true;
  end;


function handle_aix_intermediate(source: pchar; sourcecp: TSystemCodePage; out newcp: TSystemCodePage; out str: rawbytestring; len: SizeInt): boolean;
  begin
    result:=false;
    { for some reason, IBM's iconv only supports converting cp866 to/from
      ISO8859-5. This conversion is lossy, but it's better than completely
      failing. At least it keeps the cyrillic characters intact }
    case sourcecp of
      866:
        begin
          handle_aix_intermediate:=Ansi2AnsiMove(source,sourcecp,'ISO8859-5',str, len);
          if handle_aix_intermediate then
            begin
              newcp:=28595;
              setcodepage(str,newcp,false);
            end;
        end;
      28595:
        begin
          handle_aix_intermediate:=Ansi2AnsiMove(source,sourcecp,'IBM-866',str, len);
          if handle_aix_intermediate then
            begin
              newcp:=866;
              setcodepage(str,newcp,false);
            end;
        end;
    end;
  end;

