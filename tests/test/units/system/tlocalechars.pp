{ UnicodeFromLocaleChars and LocaleCharsFromUnicode follow the Delphi/Windows buffer rules, issue #29514 }
program tlocalechars;

{$mode objfpc}{$h+}
{$codepage utf8}

uses
{$ifdef unix}
  cwstring,
{$endif}
  SysUtils;

var
  gErrors: Integer;


// Reports a failed check
procedure Check(aCondition: Boolean; const aMessage: string);

begin
  if not aCondition then
    begin
    writeln('FAIL: ',aMessage);
    Inc(gErrors);
    end;
end;


// Runs the conversions with the current widestring manager; aPath names it in messages
procedure RunChecks(const aPath: string; aExact: Boolean);

const
  cUtf8: array[0..6] of AnsiChar = ('h',#$C3,#$A9,'l','l','o',#0);
var
  lW: array[0..15] of WideChar;
  lA: array[0..15] of AnsiChar;
  lSrc: UnicodeString;
  lUsed: LongBool;
  lDefault: AnsiChar;
  lRes: SizeInt;

begin
  lRes:=UnicodeFromLocaleChars(CP_UTF8,0,@cUtf8[0],6,nil,0);
  Check(lRes=5,aPath+': UnicodeFromLocaleChars required size is '+IntToStr(lRes)+', expected 5');
  FillChar(lW,SizeOf(lW),$FF);
  lRes:=UnicodeFromLocaleChars(CP_UTF8,0,@cUtf8[0],6,@lW[0],5);
  Check(lRes=5,aPath+': UnicodeFromLocaleChars wrote '+IntToStr(lRes)+' characters, expected 5');
  Check((lW[0]='h') and (lW[1]=#$E9) and (lW[4]='o'),aPath+': UnicodeFromLocaleChars converted wrongly');
  Check(Word(lW[5])=$FFFF,aPath+': UnicodeFromLocaleChars wrote past the converted characters');
  lRes:=UnicodeFromLocaleChars(CP_UTF8,0,@cUtf8[0],6,@lW[0],4);
  Check(lRes=0,aPath+': UnicodeFromLocaleChars with a too small buffer returned '+IntToStr(lRes)+', expected 0');
  lRes:=UnicodeFromLocaleChars(CP_UTF8,0,@cUtf8[0],-1,@lW[0],16);
  Check((lRes=6) and (lW[5]=#0),aPath+': UnicodeFromLocaleChars with length -1 returned '+IntToStr(lRes)+', expected 6 including the terminator');
  lRes:=UnicodeFromLocaleChars(CP_UTF8,0,@cUtf8[0],0,@lW[0],16);
  Check(lRes=0,aPath+': UnicodeFromLocaleChars with an empty source returned '+IntToStr(lRes)+', expected 0');

  lSrc:=UnicodeString('h')+WideChar($E9)+UnicodeString('llo');
  lRes:=LocaleCharsFromUnicode(CP_UTF8,0,PWideChar(lSrc),5,nil,0,nil,nil);
  Check(lRes=6,aPath+': LocaleCharsFromUnicode required size is '+IntToStr(lRes)+', expected 6');
  FillChar(lA,SizeOf(lA),'#');
  lRes:=LocaleCharsFromUnicode(CP_UTF8,0,PWideChar(lSrc),5,@lA[0],6,nil,nil);
  Check(lRes=6,aPath+': LocaleCharsFromUnicode wrote '+IntToStr(lRes)+' bytes, expected 6');
  Check(CompareByte(lA,cUtf8,6)=0,aPath+': LocaleCharsFromUnicode converted wrongly');
  Check(lA[6]='#',aPath+': LocaleCharsFromUnicode wrote past the converted bytes');
  lRes:=LocaleCharsFromUnicode(CP_UTF8,0,PWideChar(lSrc),5,@lA[0],5,nil,nil);
  Check(lRes=0,aPath+': LocaleCharsFromUnicode with a too small buffer returned '+IntToStr(lRes)+', expected 0');
  lRes:=LocaleCharsFromUnicode(CP_UTF8,0,PWideChar(lSrc),-1,@lA[0],16,nil,nil);
  Check((lRes=7) and (lA[6]=#0),aPath+': LocaleCharsFromUnicode with length -1 returned '+IntToStr(lRes)+', expected 7 including the terminator');

  lUsed:=True;
  lRes:=LocaleCharsFromUnicode(28591,0,PWideChar(lSrc),5,@lA[0],16,nil,@lUsed);
  Check((lRes=5) and (lA[1]=#$E9),aPath+': LocaleCharsFromUnicode to ISO-8859-1 converted wrongly');
  Check(not lUsed,aPath+': UsedDefaultChar is set for a string that maps completely');

  lSrc:='a'#$4E2D'b';
  lUsed:=False;
  lRes:=LocaleCharsFromUnicode(28591,0,PWideChar(lSrc),3,@lA[0],16,nil,@lUsed);
  Check(lRes=3,aPath+': LocaleCharsFromUnicode of an unmappable character returned '+IntToStr(lRes)+', expected 3');
  Check(lUsed,aPath+': UsedDefaultChar is not set for an unmappable character');

  if not aExact then
    exit;
  lDefault:='X';
  lRes:=LocaleCharsFromUnicode(28591,0,PWideChar(lSrc),3,@lA[0],16,@lDefault,@lUsed);
  Check((lRes=3) and (lA[0]='a') and (lA[1]='X') and (lA[2]='b'),aPath+': DefaultChar is not used for an unmappable character');
  lSrc:='a'#$D83D#$DE00'b';
  lRes:=LocaleCharsFromUnicode(28591,0,PWideChar(lSrc),4,@lA[0],16,@lDefault,@lUsed);
  Check((lRes=4) and (lA[1]='X') and (lA[2]='X') and (lA[3]='b'),aPath+': an unmappable surrogate pair is not replaced by one DefaultChar per code unit, result '+IntToStr(lRes));
end;


var
  lManager, lPlain: TUnicodeStringManager;

begin
  gErrors:=0;
  GetUnicodeStringManager(lManager);
  Check(Assigned(lManager.Ansi2UnicodeBufProc) and Assigned(lManager.Unicode2AnsiBufProc),'the widestring manager has no buffer procs');
  RunChecks('buffer procs',True);
  lPlain:=lManager;
  lPlain.Ansi2UnicodeBufProc:=nil;
  lPlain.Unicode2AnsiBufProc:=nil;
  SetUnicodeStringManager(lPlain);
  RunChecks('generic',False);
  SetUnicodeStringManager(lManager);
  if gErrors<>0 then
    halt(1);
  writeln('ok');
end.
