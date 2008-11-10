{
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

{$mode objfpc}
{$inline on}

unit cwstring;

interface

procedure SetCWidestringManager;

implementation

{$linklib c}

{$if not defined(linux) and not defined(solaris) and not defined(haiku)}  // Linux (and maybe glibc platforms in general), have iconv in glibc.
 {$linklib iconv}
 {$define useiconv}
{$endif linux}

Uses
  BaseUnix,
  ctypes,
  unix,
  unixtype,
  initc;

Const
{$ifndef useiconv}
    libiconvname='c';  // is in libc under Linux.
{$else}
    libiconvname='iconv';
{$endif}

{ helper functions from libc }
function towlower(__wc:wint_t):wint_t;cdecl;external clib name 'towlower';
function towupper(__wc:wint_t):wint_t;cdecl;external clib name 'towupper';

function wcscoll (__s1:pwchar_t; __s2:pwchar_t):cint;cdecl;external clib name 'wcscoll';
function strcoll (__s1:pchar; __s2:pchar):cint;cdecl;external clib name 'strcoll';
function setlocale(category: cint; locale: pchar): pchar; cdecl; external clib name 'setlocale';
{$ifndef beos}
function mbrtowc(pwc: pwchar_t; const s: pchar; n: size_t; ps: pmbstate_t): size_t; cdecl; external clib name 'mbrtowc';
function wcrtomb(s: pchar; wc: wchar_t; ps: pmbstate_t): size_t; cdecl; external clib name 'wcrtomb';
function mbrlen(const s: pchar; n: size_t; ps: pmbstate_t): size_t; cdecl; external clib name 'mbrlen';
{$else beos}
function mbtowc(pwc: pwchar_t; const s: pchar; n: size_t): size_t; cdecl; external clib name 'mbtowc';
function wctomb(s: pchar; wc: wchar_t): size_t; cdecl; external clib name 'wctomb';
function mblen(const s: pchar; n: size_t): size_t; cdecl; external clib name 'mblen';
{$endif beos}


const
{$ifdef linux}
  __LC_CTYPE = 0;
  LC_ALL = 6;
  _NL_CTYPE_CLASS = (__LC_CTYPE shl 16);
  _NL_CTYPE_CODESET_NAME = (_NL_CTYPE_CLASS)+14;
  CODESET = _NL_CTYPE_CODESET_NAME;
{$else linux}
{$ifdef darwin}
  CODESET = 0;
  LC_ALL = 0;
{$else darwin}
{$ifdef FreeBSD} // actually FreeBSD5. internationalisation is afaik not default on 4.
  __LC_CTYPE = 0;
  LC_ALL = 0;
  _NL_CTYPE_CLASS = (__LC_CTYPE shl 16);
  _NL_CTYPE_CODESET_NAME = (_NL_CTYPE_CLASS)+14;
  CODESET = 0; // _NL_CTYPE_CODESET_NAME;
{$else freebsd}
{$ifdef solaris}
  CODESET=49;
  LC_ALL = 6;
{$else solaris}
{$ifdef beos}
  {$warning check correct value for BeOS}
  CODESET=49;
  LC_ALL = 6; // Checked for BeOS, but 0 under Haiku...
  ESysEILSEQ = EILSEQ;
{$else}
{$error lookup the value of CODESET in /usr/include/langinfo.h, and the value of LC_ALL in /usr/include/locale.h for your OS }
// and while doing it, check if iconv is in libc, and if the symbols are prefixed with iconv_ or libiconv_
{$endif beos}
{$endif solaris}
{$endif FreeBSD}
{$endif darwin}
{$endif linux}

{ unicode encoding name }
{$ifdef FPC_LITTLE_ENDIAN}
  unicode_encoding2 = 'UTF-16LE';
  unicode_encoding4 = 'UCS-4LE';
{$else  FPC_LITTLE_ENDIAN}
  unicode_encoding2 = 'UTF-16BE';
  unicode_encoding4 = 'UCS-4BE';
{$endif  FPC_LITTLE_ENDIAN}

{ en_US.UTF-8 needs maximally 6 chars, UCS-4/UTF-32 needs 4   }
{ -> 10 should be enough? Should actually use MB_CUR_MAX, but }
{ that's a libc macro mapped to internal functions/variables  }
{ and thus not a stable external API on systems where libc    }
{ breaks backwards compatibility every now and then           }
  MB_CUR_MAX = 10;

type
  piconv_t = ^iconv_t;
  iconv_t = pointer;
  nl_item = cint;
{$ifndef beos}
function nl_langinfo(__item:nl_item):pchar;cdecl;external libiconvname name 'nl_langinfo';
{$endif}

{$if (not defined(bsd) and not defined(beos)) or defined(darwin) or defined(haiku)}
function iconv_open(__tocode:pchar; __fromcode:pchar):iconv_t;cdecl;external libiconvname name 'iconv_open';
function iconv(__cd:iconv_t; __inbuf:ppchar; __inbytesleft:psize_t; __outbuf:ppchar; __outbytesleft:psize_t):size_t;cdecl;external libiconvname name 'iconv';
function iconv_close(__cd:iconv_t):cint;cdecl;external libiconvname name 'iconv_close';
{$else}
function iconv_open(__tocode:pchar; __fromcode:pchar):iconv_t;cdecl;external libiconvname name 'libiconv_open';
function iconv(__cd:iconv_t; __inbuf:ppchar; __inbytesleft:psize_t; __outbuf:ppchar; __outbytesleft:psize_t):size_t;cdecl;external libiconvname name 'libiconv';
function iconv_close(__cd:iconv_t):cint;cdecl;external libiconvname name 'libiconv_close';
{$endif}

procedure fpc_rangeerror; [external name 'FPC_RANGEERROR'];


threadvar
  iconv_ansi2wide,
  iconv_wide2ansi : iconv_t;

{$ifdef beos}
function nl_langinfo(__item:nl_item):pchar;
begin
  {$warning TODO BeOS nl_langinfo or more uptodate port of iconv...}
  // Now implement the minimum required to correctly initialize WideString support
  case __item of
    CODESET : Result := 'UTF-8'; // BeOS use UTF-8
    else
    begin
      Assert(False, 'nl_langinfo was called with an unknown nl_item value');
      Result := '';
    end;
  end;
end;
{$endif}

procedure Wide2AnsiMove(source:pwidechar;var dest:ansistring;len:SizeInt);
  var
    outlength,
    outoffset,
    srclen,
    outleft : size_t;
    srcpos : pwidechar;
    destpos: pchar;
    mynil : pchar;
    my0 : size_t;
    err: cint;
  begin
    mynil:=nil;
    my0:=0;
    { rought estimation }
    setlength(dest,len*3);
    outlength:=len*3;
    srclen:=len*2;
    srcpos:=source;
    destpos:=pchar(dest);
    outleft:=outlength;
    while iconv(iconv_wide2ansi,ppchar(@srcpos),@srclen,@destpos,@outleft)=size_t(-1) do
      begin
        err:=fpgetCerrno;
        case err of
          { last character is incomplete sequence }
          ESysEINVAL,
          { incomplete sequence in the middle }
          ESysEILSEQ:
            begin
              { skip and set to '?' }
              inc(srcpos);
              dec(srclen,2);
              destpos^:='?';
              inc(destpos);
              dec(outleft);
              { reset }
              iconv(iconv_wide2ansi,@mynil,@my0,@mynil,@my0);
              if err=ESysEINVAL then
                break;
            end;
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
            runerror(231);
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
    err: cint;
  begin
    mynil:=nil;
    my0:=0;
    // extra space
    outlength:=len+1;
    setlength(dest,outlength);
    srcpos:=source;
    destpos:=pchar(dest);
    outleft:=outlength*2;
    while iconv(iconv_ansi2wide,@srcpos,psize(@len),@destpos,@outleft)=size_t(-1) do
      begin
        err:=fpgetCerrno;
        case err of
         ESysEINVAL,
         ESysEILSEQ:
            begin
              { skip and set to '?' }
              inc(srcpos);
              dec(len);
              pwidechar(destpos)^:='?';
              inc(destpos,2);
              dec(outleft,2);
              { reset }
              iconv(iconv_ansi2wide,@mynil,@my0,@mynil,@my0);
              if err=ESysEINVAL then
                break;
            end;
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
            runerror(231);
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
    for i:=0 to length(s)-1 do
      pwidechar(result)[i]:=WideChar(towlower(wint_t(s[i+1])));
  end;


function UpperWideString(const s : WideString) : WideString;
  var
    i : SizeInt;
  begin
    SetLength(result,length(s));
    for i:=0 to length(s)-1 do
      pwidechar(result)[i]:=WideChar(towupper(wint_t(s[i+1])));
  end;


procedure EnsureAnsiLen(var S: AnsiString; const len: SizeInt); inline;
begin
  if (len>length(s)) then
    if (length(s) < 10*256) then
      setlength(s,length(s)+10)
    else
      setlength(s,length(s)+length(s) shr 8);
end;


procedure ConcatCharToAnsiStr(const c: char; var S: AnsiString; var index: SizeInt);
begin
  EnsureAnsiLen(s,index);
  pchar(@s[index])^:=c;
  inc(index);
end;


{ concatenates an utf-32 char to a widestring. S *must* be unique when entering. }
{$ifndef beos}
procedure ConcatUTF32ToAnsiStr(const nc: wint_t; var S: AnsiString; var index: SizeInt; var mbstate: mbstate_t);
{$else not beos}
procedure ConcatUTF32ToAnsiStr(const nc: wint_t; var S: AnsiString; var index: SizeInt);
{$endif beos}
var
  p     : pchar;
  mblen : size_t;
begin
  { we know that s is unique -> avoid uniquestring calls}
  p:=@s[index];
  if (nc<=127) then
    ConcatCharToAnsiStr(char(nc),s,index)
  else
    begin
      EnsureAnsiLen(s,index+MB_CUR_MAX);
{$ifndef beos}
      mblen:=wcrtomb(p,wchar_t(nc),@mbstate);
{$else not beos}
      mblen:=wctomb(p,wchar_t(nc));
{$endif not beos}
      if (mblen<>size_t(-1)) then
        inc(index,mblen)
      else
        begin
          { invalid wide char }
          p^:='?';
          inc(index);
        end;
    end;
end;


function LowerAnsiString(const s : AnsiString) : AnsiString;
  var
    i, slen,
    resindex : SizeInt;
    mblen    : size_t;
{$ifndef beos}
    ombstate,
    nmbstate : mbstate_t;
{$endif beos}
    wc       : wchar_t;
  begin
{$ifndef beos}
    fillchar(ombstate,sizeof(ombstate),0);
    fillchar(nmbstate,sizeof(nmbstate),0);
{$endif beos}
    slen:=length(s);
    SetLength(result,slen+10);
    i:=1;
    resindex:=1;
    while (i<=slen) do
      begin
        if (s[i]<=#127) then
          begin
            wc:=wchar_t(s[i]);
            mblen:= 1;
          end
        else
{$ifndef beos}
          mblen:=mbrtowc(@wc, pchar(@s[i]), slen-i+1, @ombstate);
{$else not beos}
          mblen:=mbtowc(@wc, pchar(@s[i]), slen-i+1);
{$endif not beos}
        case mblen of
          size_t(-2):
            begin
              { partial invalid character, copy literally }
              while (i<=slen) do
                begin
                  ConcatCharToAnsiStr(s[i],result,resindex);
                  inc(i);
                end;
            end;
          size_t(-1), 0:
            begin
              { invalid or null character }
              ConcatCharToAnsiStr(s[i],result,resindex);
              inc(i);
            end;
          else
            begin
              { a valid sequence }
              { even if mblen = 1, the lowercase version may have a }
              { different length                                     }
              { We can't do anything special if wchar_t is 16 bit... }
{$ifndef beos}
              ConcatUTF32ToAnsiStr(towlower(wint_t(wc)),result,resindex,nmbstate);
{$else not beos}
              ConcatUTF32ToAnsiStr(towlower(wint_t(wc)),result,resindex);
{$endif not beos}
              inc(i,mblen);
            end;
          end;
      end;
    SetLength(result,resindex-1);
  end;


function UpperAnsiString(const s : AnsiString) : AnsiString;
  var
    i, slen,
    resindex : SizeInt;
    mblen    : size_t;
{$ifndef beos}
    ombstate,
    nmbstate : mbstate_t;
{$endif beos}
    wc       : wchar_t;
  begin
{$ifndef beos}
    fillchar(ombstate,sizeof(ombstate),0);
    fillchar(nmbstate,sizeof(nmbstate),0);
{$endif beos}
    slen:=length(s);
    SetLength(result,slen+10);
    i:=1;
    resindex:=1;
    while (i<=slen) do
      begin
        if (s[i]<=#127) then
          begin
            wc:=wchar_t(s[i]);
            mblen:= 1;
          end
        else
{$ifndef beos}
          mblen:=mbrtowc(@wc, pchar(@s[i]), slen-i+1, @ombstate);
{$else not beos}
          mblen:=mbtowc(@wc, pchar(@s[i]), slen-i+1);
{$endif beos}
        case mblen of
          size_t(-2):
            begin
              { partial invalid character, copy literally }
              while (i<=slen) do
                begin
                  ConcatCharToAnsiStr(s[i],result,resindex);
                  inc(i);
                end;
            end;
          size_t(-1), 0:
            begin
              { invalid or null character }
              ConcatCharToAnsiStr(s[i],result,resindex);
              inc(i);
            end;
          else
            begin
              { a valid sequence }
              { even if mblen = 1, the uppercase version may have a }
              { different length                                     }
              { We can't do anything special if wchar_t is 16 bit... }
{$ifndef beos}
              ConcatUTF32ToAnsiStr(towupper(wint_t(wc)),result,resindex,nmbstate);
{$else not beos}
              ConcatUTF32ToAnsiStr(towupper(wint_t(wc)),result,resindex);
{$endif not beos}
              inc(i,mblen);
            end;
          end;
      end;
    SetLength(result,resindex-1);
  end;


function utf16toutf32(const S: WideString; const index: SizeInt; out len: longint): UCS4Char; external name 'FPC_UTF16TOUTF32';

function WideStringToUCS4StringNoNulls(const s : WideString) : UCS4String;
  var
    i, slen,
    destindex : SizeInt;
    len       : longint;
    uch       : UCS4Char;
  begin
    slen:=length(s);
    setlength(result,slen+1);
    i:=1;
    destindex:=0;
    while (i<=slen) do
      begin
        uch:=utf16toutf32(s,i,len);
        if (uch=UCS4Char(0)) then
          uch:=UCS4Char(32);
        result[destindex]:=uch;
        inc(destindex);
        inc(i,len);
      end;
    result[destindex]:=UCS4Char(0);
    { destindex <= slen }
    setlength(result,destindex+1);
  end;


function CompareWideString(const s1, s2 : WideString) : PtrInt;
  var
    hs1,hs2 : UCS4String;
  begin
    { wcscoll interprets null chars as end-of-string -> filter out }
    hs1:=WideStringToUCS4StringNoNulls(s1);
    hs2:=WideStringToUCS4StringNoNulls(s2);
    result:=wcscoll(pwchar_t(hs1),pwchar_t(hs2));
  end;


function CompareTextWideString(const s1, s2 : WideString): PtrInt;
  begin
    result:=CompareWideString(UpperWideString(s1),UpperWideString(s2));
  end;


function CharLengthPChar(const Str: PChar): PtrInt;
  var
    nextlen: ptrint;
    s: pchar;
{$ifndef beos}
    mbstate: mbstate_t;
{$endif not beos}
  begin
    result:=0;
    s:=str;
    repeat
{$ifdef beos}
      nextlen:=ptrint(mblen(str,MB_CUR_MAX));
{$else beos}
      nextlen:=ptrint(mbrlen(str,MB_CUR_MAX,@mbstate));
{$endif beos}
      { skip invalid/incomplete sequences }
      if (nextlen<0) then
        nextlen:=1;
      inc(result,nextlen);
      inc(s,nextlen);
    until (nextlen=0);
  end;


function StrCompAnsiIntern(s1,s2 : PChar; len1, len2: PtrInt; canmodifys1, canmodifys2: boolean): PtrInt;
  var
    a,b: pchar;
    i: PtrInt;
  begin
    if not(canmodifys1) then
      getmem(a,len1+1)
    else
      a:=s1;
    for i:=0 to len1-1 do
      if s1[i]<>#0 then
        a[i]:=s1[i]
      else
        a[i]:=#32;
    a[len1]:=#0;

    if not(canmodifys2) then
      getmem(b,len2+1)
    else
      b:=s2;
    for i:=0 to len2-1 do
      if s2[i]<>#0 then
        b[i]:=s2[i]
      else
        b[i]:=#32;
    b[len2]:=#0;
    result:=strcoll(a,b);
    if not(canmodifys1) then
      freemem(a);
    if not(canmodifys2) then
      freemem(b);
  end;


function CompareStrAnsiString(const s1, s2: ansistring): PtrInt;
  begin
    result:=StrCompAnsiIntern(pchar(s1),pchar(s2),length(s1),length(s2),false,false);
  end;


function StrCompAnsi(s1,s2 : PChar): PtrInt;
  begin
    result:=strcoll(s1,s2);
  end;


function AnsiCompareText(const S1, S2: ansistring): PtrInt;
  var
    a, b: AnsiString;
  begin
    a:=UpperAnsistring(s1);
    b:=UpperAnsistring(s2);
    result:=StrCompAnsiIntern(pchar(a),pchar(b),length(a),length(b),true,true);
  end;


function AnsiStrIComp(S1, S2: PChar): PtrInt;
  begin
    result:=AnsiCompareText(ansistring(s1),ansistring(s2));
  end;


function AnsiStrLComp(S1, S2: PChar; MaxLen: PtrUInt): PtrInt;
  var
    a, b: pchar;
begin
  if (maxlen=0) then
    exit(0);
  if (s1[maxlen]<>#0) then
    begin
      getmem(a,maxlen+1);
      move(s1^,a^,maxlen);
      a[maxlen]:=#0;
    end
  else
    a:=s1;
  if (s2[maxlen]<>#0) then
    begin
      getmem(b,maxlen+1);
      move(s2^,b^,maxlen);
      b[maxlen]:=#0;
    end
  else
    b:=s2;
  result:=StrCompAnsiIntern(a,b,maxlen,maxlen,a<>s1,b<>s2);
  if (a<>s1) then
    freemem(a);
  if (b<>s2) then
    freemem(b);
end;


function AnsiStrLIComp(S1, S2: PChar; MaxLen: PtrUInt): PtrInt;
  var
    a, b: ansistring;
begin
  if (maxlen=0) then
    exit(0);
  setlength(a,maxlen);
  move(s1^,a[1],maxlen);
  setlength(b,maxlen);
  move(s2^,b[1],maxlen);
  result:=AnsiCompareText(a,b);
end;


procedure ansi2pchar(const s: ansistring; const orgp: pchar; out p: pchar);
var
  newlen: sizeint;
begin
  newlen:=length(s);
  if newlen>strlen(orgp) then
    fpc_rangeerror;
  p:=orgp;
  if (newlen>0) then
    move(s[1],p[0],newlen);
  p[newlen]:=#0;
end;


function AnsiStrLower(Str: PChar): PChar;
var
  temp: ansistring;
begin
  temp:=loweransistring(str);
  ansi2pchar(temp,str,result);
end;


function AnsiStrUpper(Str: PChar): PChar;
var
  temp: ansistring;
begin
  temp:=upperansistring(str);
  ansi2pchar(temp,str,result);
end;


procedure InitThread;
begin
  iconv_wide2ansi:=iconv_open(nl_langinfo(CODESET),unicode_encoding2);
  iconv_ansi2wide:=iconv_open(unicode_encoding2,nl_langinfo(CODESET));
end;


procedure FiniThread;
begin
  if (iconv_wide2ansi <> iconv_t(-1)) then
    iconv_close(iconv_wide2ansi);
  if (iconv_ansi2wide <> iconv_t(-1)) then
    iconv_close(iconv_ansi2wide);
end;


Procedure SetCWideStringManager;
Var
  CWideStringManager : TUnicodeStringManager;
begin
  CWideStringManager:=widestringmanager;
  With CWideStringManager do
    begin
      Wide2AnsiMoveProc:=@Wide2AnsiMove;
      Ansi2WideMoveProc:=@Ansi2WideMove;

      UpperWideStringProc:=@UpperWideString;
      LowerWideStringProc:=@LowerWideString;

      CompareWideStringProc:=@CompareWideString;
      CompareTextWideStringProc:=@CompareTextWideString;

      CharLengthPCharProc:=@CharLengthPChar;

      UpperAnsiStringProc:=@UpperAnsiString;
      LowerAnsiStringProc:=@LowerAnsiString;
      CompareStrAnsiStringProc:=@CompareStrAnsiString;
      CompareTextAnsiStringProc:=@AnsiCompareText;
      StrCompAnsiStringProc:=@StrCompAnsi;
      StrICompAnsiStringProc:=@AnsiStrIComp;
      StrLCompAnsiStringProc:=@AnsiStrLComp;
      StrLICompAnsiStringProc:=@AnsiStrLIComp;
      StrLowerAnsiStringProc:=@AnsiStrLower;
      StrUpperAnsiStringProc:=@AnsiStrUpper;
      ThreadInitProc:=@InitThread;
      ThreadFiniProc:=@FiniThread;
{$ifndef VER2_2}
      { Unicode }
      Unicode2AnsiMoveProc:=@Wide2AnsiMove;
      Ansi2UnicodeMoveProc:=@Ansi2WideMove;
      UpperUnicodeStringProc:=@UpperWideString;
      LowerUnicodeStringProc:=@LowerWideString;
{$endif VER2_2}
    end;
  SetUnicodeStringManager(CWideStringManager);
end;


initialization
  SetCWideStringManager;

  { you have to call setlocale(LC_ALL,'') to initialise the langinfo stuff  }
  { with the information from the environment variables according to POSIX  }
  { (some OSes do this automatically, but e.g. Darwin and Solaris don't)    }
  setlocale(LC_ALL,'');

  { init conversion tables for main program }
  InitThread;
finalization
  { fini conversion tables for main program }
  FiniThread;
end.

