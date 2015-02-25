{
    This file is part of the Free Pascal run time library.
    Copyright (c) 2015 by Jonas Maebe,
    member of the Free Pascal development team.

    CoreFoundation-based wide string support

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
 **********************************************************************}

{$mode objfpc}
{$implicitexceptions off}

unit iosxwstr;

interface

{$linkframework CoreFoundation}

  procedure SetCFWidestringManager;

implementation

  uses
    unixcp,
    { for access to libiconv-based routines }
    cwstring,
    MacTypes,
    CFBase,CFString,CFStringEncodingExt,CFLocale;

  procedure fpc_rangeerror; [external name 'FPC_RANGEERROR'];

  var
    CWStringWideStringManager: TUnicodeStringManager;

  procedure InitThread;
    begin
      { we don't need anything special, but since we may use cwstring itself,
        call through to it }
      CWStringWideStringManager.ThreadInitProc;
    end;


  procedure FiniThread;
    begin
      { we don't need anything special, but since we may use cwstring itself,
        call through to it }
      CWStringWideStringManager.ThreadFiniProc;
    end;


  function get_cfencoding_for_cp(cp: TSystemCodePage): CFStringEncoding;
    var
      defscp: TSystemCodePage;
    begin
      { translate placeholder code pages }
      if (cp=CP_ACP) or
         (cp=CP_OEMCP) then
        cp:=DefaultSystemCodePage;
      result:=CFStringConvertWindowsCodepageToEncoding(cp);
    end;


  procedure GetAnsiDataFromCFString(str: CFstringRef; cfcp: CFStringEncoding; estimated_length: SizeInt; var dest: RawByteString);
    var
      range: CFRange;
      encodedlen,convertedchars: CFIndex;
      strlen: SizeInt;
    begin
      { first rough estimate for the length }
      setlength(dest,estimated_length);
      { try to convert }
      range.location:=0;
      strlen:=CFStringGetLength(str);
      range.length:=strlen;
      convertedchars:=CFStringGetBytes(str,range,cfcp,ByteParameter('?'),false,UInt8Ptr(dest),estimated_length,encodedlen);
      { failed -> bail out }
      if convertedchars<0 then
        begin
          CFRelease(str);
          runerror(231);
        end
      { if partially succesful, recreate with the required len }
      else if convertedchars<strlen then
        begin
          setlength(dest,encodedlen);
          { try again }
          convertedchars:=CFStringGetBytes(str,range,cfcp,ByteParameter('?'),false,UInt8Ptr(dest),encodedlen,encodedlen);
          { failed again ? }
          if convertedchars<>strlen then
            begin
              CFRelease(str);
              runerror(231);
            end;
        end;
      { truncate }
      setlength(dest,encodedlen);
    end;


  function CFStringCreateFromAnsiData(data: pchar; len: SizeInt; cp: TSystemCodePage): CFStringRef;
    var
      strlen,encodedlen: CFIndex;
      range: CFRange;
      cfcp: CFStringEncoding;
    begin
      result:=nil;
      { get source cf codepage }
      cfcp:=get_cfencoding_for_cp(cp);
      { unsupported encoding -> try libiconv instead }
      if cfcp=kCFStringEncodingInvalidId then
        exit;
      { make a cfstring from the original data }
      result:=CFStringCreateWithBytesNoCopy(nil,UnivPtr(data),len,cfcp,false,kCFAllocatorNull);
    end;


  function CFStringCreateFromAnsiDataOptionallyViaUnicodeString(data: pchar; len: SizeInt; cp: TSystemCodePage; out wtemp: UnicodeString): CFStringRef;
    begin
      result:=CFStringCreateFromAnsiData(data,len,cp);
      { failed -> translate via libiconv and then create using the unicodestring
        characters; since we use the no-copy constructor for performance
        reasons, the unicodestring has to survive this routine }
      if not assigned(result) then
        begin
          CWStringWideStringManager.Ansi2UnicodeMoveProc(data,cp,wtemp,len);
          result:=CFStringCreateWithCharactersNoCopy(nil,UniCharPtr(wtemp),len,kCFAllocatorNull);
        end;
    end;


  function CFStringCreateFromWideData(data: pwidechar; len: SizeInt): CFStringRef; inline;
    begin
      { make a cfstring from the utf-16 data }
      result:=CFStringCreateWithCharactersNoCopy(nil,UniCharPtr(data),len,kCFAllocatorNull);
    end;


  function CFStringCreateFromWideDataOptionallyViaUUTF8String(data: pwidechar; len: SizeInt; out temp: RawByteString): CFStringRef;
    begin
      result:=CFStringCreateFromWideData(data,len);
      { failed -> translate to UTF-8 via libiconv to filter out any
        potentially invalid characters and then create using the unicodestring
        characters; since we use the no-copy constructor for performance
        reasons, the unicodestring has to survive this routine }
      if not assigned(result) then
        begin
          CWStringWideStringManager.Unicode2AnsiMoveProc(data,temp,CP_UTF8,len);
          result:=CFStringCreateWithBytesNoCopy(nil,UnivPtr(temp),length(temp),kCFStringEncodingUTF8,false,kCFAllocatorNull);
          if not assigned(result) then
            runerror(231)
        end;
    end;


  procedure Wide2AnsiMove(source:pwidechar; var dest:RawByteString; cp:TSystemCodePage; len:SizeInt);
    var
      str: CFStringRef;
      strlen,estimatedlen: CFIndex;
      cfcp: CFStringEncoding;
    begin
      str:=nil;
      { get target cf codepage }
      cfcp:=get_cfencoding_for_cp(cp);
      { unsupported encoding -> default move }
      if cfcp<>kCFStringEncodingInvalidId then
        { make a cfstring from the utf-16 data }
        str:=CFStringCreateFromWideData(source,len);
      { You cannot create a CFString for a sequence that contains an error :/
        We want to replace the error positions with '?' -> fall back to libiconv
      }
      if not assigned(str) then
        begin
          CWStringWideStringManager.Unicode2AnsiMoveProc(source,dest,cp,len);
          exit;
        end;

      GetAnsiDataFromCFString(str,cfcp,len*3,dest);
      { set code page }
      SetCodePage(dest,cp,false);
      { release cfstring }
      CFRelease(str);
    end;


  procedure Ansi2WideMove(source:pchar; cp:TSystemCodePage; var dest:widestring; len:SizeInt);
    var
      str: CFStringRef;
      strlen,encodedlen: CFIndex;
      range: CFRange;
      cfcp: CFStringEncoding;
    begin
      str:=CFStringCreateFromAnsiData(source,len,cp);
      { You cannot create a CFString for a sequence that contains an error :/
        We want to replace the error positions with '?' -> fall back to libiconv
      }
      if not assigned(str) then
        begin
          CWStringWideStringManager.Ansi2UnicodeMoveProc(source,cp,dest,len);
          exit;
        end;

      { convert }
      range.location:=0;
      strlen:=CFStringGetLength(str);
      range.length:=strlen;
      setlength(dest,strlen);
      CFStringGetCharacters(str,range,UniCharPtr(dest));
      { release cfstring }
      CFRelease(str);
    end;


  function LowerWideString(const s : WideString) : WideString;
    var
      str: CFStringRef;
      mstr: CFMutableStringRef;
      range: CFRange;
      encodedlen: CFIndex;
      locale: CFLocaleRef;
      temp: RawByteString;
    begin
      { empty string -> exit }
      if s='' then
        begin
          result:='';
          exit;
        end;
      { create cfstring from the string data }
      str:=CFStringCreateFromWideDataOptionallyViaUUTF8String(pwidechar(s),length(s),temp);
      { convert to mutable cfstring }
      mstr:=CFStringCreateMutableCopy(nil,0,str);
      { lowercase }
      locale:=CFLocaleCopyCurrent;
      CFStringLowercase(mstr,CFLocaleCopyCurrent);
      CFRelease(locale);
      { extract the data again }
      range.location:=0;
      range.length:=CFStringGetLength(CFStringRef(mstr));
      setlength(result,range.length);
      CFStringGetCharacters(mstr,range,UniCharPtr(result));
      CFRelease(mstr);
      CFRelease(str);
    end;


  function UpperWideString(const s : WideString) : WideString;
  var
    str: CFStringRef;
    mstr: CFMutableStringRef;
    range: CFRange;
    encodedlen: CFIndex;
    locale: CFLocaleRef;
    temp: RawByteString;
  begin
    { empty string -> exit }
    if s='' then
      begin
        result:='';
        exit;
      end;
    { create cfstring from the string data }
    str:=CFStringCreateFromWideDataOptionallyViaUUTF8String(pwidechar(s),length(s),temp);
    { convert to mutable cfstring }
    mstr:=CFStringCreateMutableCopy(nil,0,str);
    { lowercase }
    locale:=CFLocaleCopyCurrent;
    CFStringUppercase(mstr,locale);
    CFRelease(locale);
    { extract the data again }
    range.location:=0;
    range.length:=CFStringGetLength(CFStringRef(mstr));
    setlength(result,range.length);
    CFStringGetCharacters(mstr,range,UniCharPtr(result));
    CFRelease(mstr);
    CFRelease(str);
  end;


  function UpperLowerAnsiString(const s: AnsiString; upper: boolean): AnsiString;
    var
      str: CFStringRef;
      mstr: CFMutableStringRef;
      cfcp: CFStringEncoding;
      locale: CFLocaleRef;
      wtemp: UnicodeString;
      range: CFRange;
    begin
      if s='' then
        begin
          result:='';
          exit
        end;
      str:=CFStringCreateFromAnsiDataOptionallyViaUnicodeString(pchar(s),length(s),StringCodePage(s),wtemp);
      { unsupported encoding for either CF or iconv -> return original string }
      if not assigned(str) then
        begin
          result:=s;
          exit;
        end;
      { convert to mutable cfstring }
      mstr:=CFStringCreateMutableCopy(nil,0,str);
      CFRelease(str);
      { upper/lowercase }
      locale:=CFLocaleCopyCurrent;
      if upper then
        CFStringUppercase(mstr,locale)
      else
        CFStringLowercase(mstr,locale);
      CFRelease(locale);
      { convert back to ansistring }
      cfcp:=get_cfencoding_for_cp(StringCodePage(s));
      if cfcp<>kCFStringEncodingInvalidId then
        begin
          GetAnsiDataFromCFString(CFStringRef(mstr),cfcp,length(s),RawByteString(result));
          SetCodePage(RawByteString(result),StringCodePage(s),false);
        end
      else
        begin
          { unsupported encoding -> use libiconv instead via UTF-16
            intermediate }
          range.location:=0;
          range.length:=CFStringGetLength(mstr);
          SetLength(wtemp,range.length);
          CFStringGetCharacters(mstr,range,UniCharPtr(wtemp));
          CWStringWideStringManager.Wide2AnsiMoveProc(pwidechar(wtemp),RawByteString(result),StringCodePage(s),range.length);
        end;
      CFRelease(mstr);
    end;


  function LowerAnsiString(const s: AnsiString): AnsiString;
    begin
      result:=UpperLowerAnsiString(s,false);
    end;


  function UpperAnsiString(const s: AnsiString): AnsiString;
    begin
      result:=UpperLowerAnsiString(s,true);
    end;


  function CompareCFStrings(const s1, s2: CFStringRef; case_insensitive: boolean): longint;
    var
      flags: CFStringCompareFlags;
    begin
      flags:=0;
      if case_insensitive then
        flags:=flags or kCFCompareCaseInsensitive;
      result:=CFStringCompare(s1,s2,flags)
    end;


  function CompareWideString(const s1, s2 : WideString) : PtrInt;
    var
      cfstr1, cfstr2: CFStringRef;
      temp1, temp2: RawByteString;
    begin
      cfstr1:=CFStringCreateFromWideDataOptionallyViaUUTF8String(pwidechar(s1),length(s1),temp1);
      cfstr2:=CFStringCreateFromWideDataOptionallyViaUUTF8String(pwidechar(s2),length(s2),temp2);
      result:=CompareCFStrings(cfstr1,cfstr2,false);
      CFRelease(cfstr1);
      CFRelease(cfstr2);
    end;

  function CompareTextWideString(const s1, s2 : WideString): PtrInt;
    var
      cfstr1, cfstr2: CFStringRef;
      temp1, temp2: RawByteString;
    begin
      cfstr1:=CFStringCreateFromWideDataOptionallyViaUUTF8String(pwidechar(s1),length(s1),temp1);
      cfstr2:=CFStringCreateFromWideDataOptionallyViaUUTF8String(pwidechar(s2),length(s2),temp2);
      result:=CompareCFStrings(cfstr1,cfstr2,true);
      CFRelease(cfstr1);
      CFRelease(cfstr2);
    end;


  function InternalCodePointLength(const Str: PChar; cfcp: CFStringEncoding; maxlookahead: ptrint): PtrInt;
    var
      cfstr: CFStringRef;
    begin
      result:=0;
      { try creating a string with the first 1, 2, ... bytes until we find a
        valid one }
      while (str[result]<>#0) and
            (result<maxlookahead) do
        begin
          inc(result);
          cfstr:=CFStringCreateWithBytesNoCopy(nil,UnivPtr(Str),result,cfcp,false,kCFAllocatorNull);
          if assigned(cfstr) then
            begin
              CFRelease(cfstr);
              exit;
            end;
        end;
      result:=-1;
    end;


  function CharLengthPChar(const Str: PChar): PtrInt;
    var
      cfstr: CFStringRef;
      cfcp: CFStringEncoding;
      s: PChar;
      tmplen: PtrInt;
    begin
      result:=0;
      if str[0]=#0 then
        exit;
      cfcp:=get_cfencoding_for_cp(DefaultSystemCodePage);
      if cfcp=kCFStringEncodingInvalidId then
        begin
          { or -1? }
          result:=strlen(Str);
          exit
        end;
      s:=str;
      repeat
        tmplen:=InternalCodePointLength(s,cfcp,8);
        { invalid -> skip }
        if tmplen=-1 then
          tmplen:=1;
        inc(s,tmplen);
        inc(result);
      until s[0]=#0;
    end;


  function CodePointLength(const Str: PChar; maxlookahead: ptrint): PtrInt;
    var
      cfstr: CFStringRef;
      cfcp: CFStringEncoding;
    begin
      result:=0;
      if str[0]=#0 then
        exit;
      cfcp:=get_cfencoding_for_cp(DefaultSystemCodePage);
      if cfcp=kCFStringEncodingInvalidId then
        begin
          { if we would return -1, then the caller would keep trying with
            longer and longer sequences, but that wouldn't change anything }
          result:=1;
          exit
        end;
      result:=InternalCodePointLength(str,cfcp,maxlookahead);
    end;


  function CompareStrAnsiString(const s1, s2: ansistring): PtrInt;
    var
      cfstr1, cfstr2: CFStringRef;
      wtemp1, wtemp2: UnicodeString;
    begin
      cfstr1:=CFStringCreateFromAnsiDataOptionallyViaUnicodeString(pchar(s1),length(s1),StringCodePage(s1),wtemp1);
      cfstr2:=CFStringCreateFromAnsiDataOptionallyViaUnicodeString(pchar(s2),length(s2),StringCodePage(s2),wtemp2);
      result:=CompareCFStrings(cfstr1,cfstr2,false);
      CFRelease(cfstr1);
      CFRelease(cfstr2);
    end;


  function StrCompAnsi(s1,s2 : PChar): PtrInt;
    var
      cfstr1, cfstr2: CFStringRef;
      wtemp1, wtemp2: UnicodeString;
    begin
      cfstr1:=CFStringCreateFromAnsiDataOptionallyViaUnicodeString(s1,strlen(s1),DefaultSystemCodePage,wtemp1);
      cfstr2:=CFStringCreateFromAnsiDataOptionallyViaUnicodeString(s2,strlen(s2),DefaultSystemCodePage,wtemp2);
      result:=CompareCFStrings(cfstr1,cfstr2,false);
      CFRelease(cfstr1);
      CFRelease(cfstr2);
    end;


  function AnsiCompareText(const S1, S2: ansistring): PtrInt;
    var
      cfstr1, cfstr2: CFStringRef;
      wtemp1, wtemp2: UnicodeString;
    begin
      cfstr1:=CFStringCreateFromAnsiDataOptionallyViaUnicodeString(pchar(s1),length(s1),DefaultSystemCodePage,wtemp1);
      cfstr2:=CFStringCreateFromAnsiDataOptionallyViaUnicodeString(pchar(s2),length(s2),DefaultSystemCodePage,wtemp2);
      result:=CompareCFStrings(cfstr1,cfstr2,true);
      CFRelease(cfstr1);
      CFRelease(cfstr2);
    end;


  function AnsiStrIComp(S1, S2: PChar): PtrInt;
    var
      cfstr1, cfstr2: CFStringRef;
      wtemp1, wtemp2: UnicodeString;
    begin
      cfstr1:=CFStringCreateFromAnsiDataOptionallyViaUnicodeString(s1,strlen(s1),DefaultSystemCodePage,wtemp1);
      cfstr2:=CFStringCreateFromAnsiDataOptionallyViaUnicodeString(s2,strlen(s2),DefaultSystemCodePage,wtemp2);
      result:=CompareCFStrings(cfstr1,cfstr2,true);
      CFRelease(cfstr1);
      CFRelease(cfstr2);
    end;


  function AnsiStrLComp(S1, S2: PChar; MaxLen: PtrUInt): PtrInt;
    var
      cfstr1, cfstr2: CFStringRef;
      wtemp1, wtemp2: UnicodeString;
    begin
      cfstr1:=CFStringCreateFromAnsiDataOptionallyViaUnicodeString(s1,MaxLen,StringCodePage(s1),wtemp1);
      cfstr2:=CFStringCreateFromAnsiDataOptionallyViaUnicodeString(s2,MaxLen,StringCodePage(s2),wtemp2);
      result:=CompareCFStrings(cfstr1,cfstr2,false);
      CFRelease(cfstr1);
      CFRelease(cfstr2);
    end;


  function AnsiStrLIComp(S1, S2: PChar; MaxLen: PtrUInt): PtrInt;
    var
      cfstr1, cfstr2: CFStringRef;
      wtemp1, wtemp2: UnicodeString;
    begin
      cfstr1:=CFStringCreateFromAnsiDataOptionallyViaUnicodeString(s1,MaxLen,StringCodePage(s1),wtemp1);
      cfstr2:=CFStringCreateFromAnsiDataOptionallyViaUnicodeString(s2,MaxLen,StringCodePage(s2),wtemp2);
      result:=CompareCFStrings(cfstr1,cfstr2,true);
      CFRelease(cfstr1);
      CFRelease(cfstr2);
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


  function GetStandardCodePage(const stdcp: TStandardCodePageEnum): TSystemCodePage;
    var
      langinfo: pchar;
    begin
      { don't use CFStringGetSystemEncoding, that one returns MacRoman on e.g.
        an English system, which is definitely not what we want. Since there are
        no "ansi" interfaces on OS X and all APIs support all characters, always
        use UTF-8. Additionally,  Darwin always uses UTF-8 for file system
        operations }
      result:=CP_UTF8;
    end;


  procedure SetStdIOCodePage(var T: Text); inline;
    begin
      case TextRec(T).Mode of
        fmInput:TextRec(T).CodePage:=GetStandardCodePage(scpConsoleInput);
        fmOutput:TextRec(T).CodePage:=GetStandardCodePage(scpConsoleOutput);
      end;
    end;


  procedure SetStdIOCodePages; inline;
    begin
      SetStdIOCodePage(Input);
      SetStdIOCodePage(Output);
      SetStdIOCodePage(ErrOutput);
      SetStdIOCodePage(StdOut);
      SetStdIOCodePage(StdErr);
    end;


  procedure SetCFWideStringManager;
    var
      CFWideStringManager : TUnicodeStringManager;
    begin
      GetUnicodeStringManager(CWStringWideStringManager);
      CFWideStringManager:=CWStringWideStringManager;
      with CFWideStringManager do
        begin
          Wide2AnsiMoveProc:=@Wide2AnsiMove;
          Ansi2WideMoveProc:=@Ansi2WideMove;

          UpperWideStringProc:=@UpperWideString;
          LowerWideStringProc:=@LowerWideString;

          CompareWideStringProc:=@CompareWideString;
          CompareTextWideStringProc:=@CompareTextWideString;

          CharLengthPCharProc:=@CharLengthPChar;
          CodePointLengthProc:=@CodePointLength;

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
          { Unicode }
          Unicode2AnsiMoveProc:=@Wide2AnsiMove;
          Ansi2UnicodeMoveProc:=@Ansi2WideMove;
          UpperUnicodeStringProc:=@UpperWideString;
          LowerUnicodeStringProc:=@LowerWideString;
          CompareUnicodeStringProc:=@CompareWideString;
          CompareTextUnicodeStringProc:=@CompareTextWideString;
          { CodePage }
          GetStandardCodePageProc:=@GetStandardCodePage;
        end;
      SetUnicodeStringManager(CFWideStringManager);
    end;


initialization
  SetCFWideStringManager;

  { set the DefaultSystemCodePage }
  DefaultSystemCodePage:=GetStandardCodePage(scpAnsi);
  DefaultFileSystemCodePage:=GetStandardCodePage(scpFileSystemSingleByte);
  DefaultRTLFileSystemCodePage:=DefaultFileSystemCodePage;

  SetStdIOCodePages;

  { don't call init, we don't need to do anything and the cwstring routine we
    call through has already been called from the init code of cwstring itself
  InitThread;
  }
finalization
  { don't call for the same reason as not calling FiniThread
  FiniThread;
  }
  { restore previous widestring manager so that subsequent calls
    into the widestring manager won't trigger the finalized functionality }
  SetWideStringManager(CWStringWideStringManager);
end.
