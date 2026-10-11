program tw41110;

{$mode objfpc}{$h+}

uses
  SysUtils, StrUtils;

type
{$if sizeof(SizeInt)<sizeof(Integer)}
  TReplaceCount = Integer;
{$else}
  TReplaceCount = SizeInt;
{$endif}

var
  gFailed: Boolean = False;


// Reports aMessage as a failure when aCondition is false
procedure Check(aCondition: Boolean; const aMessage: string);

begin
  if not aCondition then
    begin
    Writeln('failed: ',aMessage);
    gFailed:=True;
    end;
end;


var
  lA: AnsiString;
  lU: UnicodeString;
  lW: WideString;
  lCount: TReplaceCount;
  lIntCount: Integer;
  lI1, lI2: Integer;

begin
  lA:=StringReplace('abcabcabc','b','XY',[rfReplaceAll],lCount);
  Check((lA='aXYcaXYcaXYc') and (lCount=3),'AnsiString StringReplace with SizeInt count');
  lA:=StringReplace('abcabcabc','B','x',[rfReplaceAll,rfIgnoreCase],lIntCount);
  Check((lA='axcaxcaxc') and (lIntCount=3),'AnsiString StringReplace with Integer count');
  lA:=StringReplace('abcabc','b','',[],lCount);
  Check((lA='acabc') and (lCount=1),'AnsiString StringReplace first occurrence with SizeInt count');

{$if sizeof(Char)=1}
  lA:=StringReplace('abcabcabc','b','XY',[rfReplaceAll],lCount,sraManySmall);
  Check((lA='aXYcaXYcaXYc') and (lCount=3),'StrUtils StringReplace sraManySmall with SizeInt count');
  lA:=StringReplace('abcabcabc','B','x',[rfReplaceAll,rfIgnoreCase],lIntCount,sraManySmall);
  Check((lA='axcaxcaxc') and (lIntCount=3),'StrUtils StringReplace sraManySmall with Integer count');
  lA:=StringReplace('abcabcabc','bc','',[rfReplaceAll],lCount,sraBoyerMoore);
  Check((lA='aaa') and (lCount=3),'StrUtils StringReplace sraBoyerMoore with SizeInt count');
  lA:=StringReplace('abcabcabc','BC','',[rfIgnoreCase],lIntCount,sraBoyerMoore);
  Check((lA='aabcabc') and (lIntCount=1),'StrUtils StringReplace sraBoyerMoore with Integer count');
{$endif}
  lA:=StringReplace('abcabc','c','Z',[rfReplaceAll],lCount,sraDefault);
  Check((lA='abZabZ') and (lCount=2),'StrUtils StringReplace sraDefault with SizeInt count');

  lU:=StringReplace(UnicodeString('abcabc'),UnicodeString('c'),UnicodeString('Z'),[rfReplaceAll],lCount);
  Check((lU='abZabZ') and (lCount=2),'UnicodeString StringReplace with SizeInt count');
  lU:=StringReplace(UnicodeString('abcabc'),UnicodeString('c'),UnicodeString('Z'),[rfReplaceAll],lIntCount);
  Check((lU='abZabZ') and (lIntCount=2),'UnicodeString StringReplace with Integer count');
  lU:=UnicodeStringReplace('abcabc','a','',[rfReplaceAll],lCount);
  Check((lU='bcbc') and (lCount=2),'UnicodeStringReplace with SizeInt count');

  lW:=WideStringReplace('abcabc','bc','Q',[rfReplaceAll],lCount);
  Check((lW='aQaQ') and (lCount=2),'WideStringReplace with SizeInt count');
  lW:=WideStringReplace('abcabc','bc','Q',[rfReplaceAll],lIntCount);
  Check((lW='aQaQ') and (lIntCount=2),'WideStringReplace with Integer count');

  Check(SizeOf(ByteLength(lU))=SizeOf(SizeInt),'ByteLength returns SizeInt');
  Check(ByteLength(lU)=4*SizeOf(UnicodeChar),'ByteLength of a 4 character string');

  Check(SizeOf(SScanf('','',[]))=SizeOf(SizeInt),'SScanf returns SizeInt');
  Check(SScanf('12 34','%d %d',[@lI1,@lI2])=2,'SScanf returns the number of converted values');
  Check((lI1=12) and (lI2=34),'SScanf converts both values');

  if gFailed then
    Halt(1);
  Writeln('ok');
end.
