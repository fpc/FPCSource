{$mode objfpc}
{$h+}
{
    $Id$
    This file is part of the Free Pascal run time library.
    Copyright (c) 1999-2000 by the Free Pascal development team

    Delphi/Kylix compatibility unit: String handling routines. 
    
    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}
unit strutils;

interface

uses
  SysUtils{, Types};

{ ---------------------------------------------------------------------
    Case sensitive search/replace
  ---------------------------------------------------------------------}

Function AnsiResemblesText(const AText, AOther: string): Boolean;
Function AnsiContainsText(const AText, ASubText: string): Boolean;
Function AnsiStartsText(const ASubText, AText: string): Boolean;
Function AnsiEndsText(const ASubText, AText: string): Boolean;
Function AnsiReplaceText(const AText, AFromText, AToText: string): string;
Function AnsiMatchText(const AText: string; const AValues: array of string): Boolean;
Function AnsiIndexText(const AText: string; const AValues: array of string): Integer;

{ ---------------------------------------------------------------------
    Case insensitive search/replace
  ---------------------------------------------------------------------}

Function AnsiContainsStr(const AText, ASubText: string): Boolean;
Function AnsiStartsStr(const ASubText, AText: string): Boolean;
Function AnsiEndsStr(const ASubText, AText: string): Boolean;
Function AnsiReplaceStr(const AText, AFromText, AToText: string): string;
Function AnsiMatchStr(const AText: string; const AValues: array of string): Boolean;
Function AnsiIndexStr(const AText: string; const AValues: array of string): Integer;

{ ---------------------------------------------------------------------
    Playthingies
  ---------------------------------------------------------------------}

Function DupeString(const AText: string; ACount: Integer): string;
Function ReverseString(const AText: string): string;
Function AnsiReverseString(const AText: AnsiString): AnsiString;
Function StuffString(const AText: string; AStart, ALength: Cardinal;  const ASubText: string): string;
Function RandomFrom(const AValues: array of string): string; overload;
Function IfThen(AValue: Boolean; const ATrue: string; AFalse: string): string; 
Function IfThen(AValue: Boolean; const ATrue: string): string; // ; AFalse: string = ''

{ ---------------------------------------------------------------------
    VB emulations.
  ---------------------------------------------------------------------}

Function LeftStr(const AText: AnsiString; const ACount: Integer): AnsiString; 
Function RightStr(const AText: AnsiString; const ACount: Integer): AnsiString; 
Function MidStr(const AText: AnsiString; const AStart, ACount: Integer): AnsiString; 
Function RightBStr(const AText: AnsiString; const AByteCount: Integer): AnsiString;
Function MidBStr(const AText: AnsiString; const AByteStart, AByteCount: Integer): AnsiString;
Function AnsiLeftStr(const AText: AnsiString; const ACount: Integer): AnsiString;
Function AnsiRightStr(const AText: AnsiString; const ACount: Integer): AnsiString;
Function AnsiMidStr(const AText: AnsiString; const AStart, ACount: Integer): AnsiString;
{$ifndef ver1_0}
Function LeftBStr(const AText: AnsiString; const AByteCount: Integer): AnsiString;
Function LeftStr(const AText: WideString; const ACount: Integer): WideString; 
Function RightStr(const AText: WideString; const ACount: Integer): WideString; 
Function MidStr(const AText: WideString; const AStart, ACount: Integer): WideString; 
{$endif}

{ ---------------------------------------------------------------------
    Extended search and replace
  ---------------------------------------------------------------------}
  
const
  { Default word delimiters are any character except the core alphanumerics. }
  WordDelimiters: set of Char = [#0..#255] - ['a'..'z','A'..'Z','1'..'9','0'];

type
  TStringSeachOption = (soDown, soMatchCase, soWholeWord);
  TStringSearchOptions = set of TStringSeachOption;

Function SearchBuf(Buf: PChar; BufLen: Integer; SelStart, SelLength: Integer; SearchString: String; Options: TStringSearchOptions): PChar;
Function SearchBuf(Buf: PChar; BufLen: Integer; SelStart, SelLength: Integer; SearchString: String): PChar; // ; Options: TStringSearchOptions = [soDown]
Function PosEx(const SubStr, S: string; Offset: Cardinal): Integer;
Function PosEx(const SubStr, S: string): Integer; // Offset: Cardinal = 1
Function PosEx(c:char; const S: string; Offset: Cardinal): Integer;

{ ---------------------------------------------------------------------
    Soundex Functions.
  ---------------------------------------------------------------------}

type
  TSoundexLength = 1..MaxInt;

Function Soundex(const AText: string; ALength: TSoundexLength): string;
Function Soundex(const AText: string): string; // ; ALength: TSoundexLength = 4

type
  TSoundexIntLength = 1..8;

Function SoundexInt(const AText: string; ALength: TSoundexIntLength): Integer;
Function SoundexInt(const AText: string): Integer; //; ALength: TSoundexIntLength = 4
Function DecodeSoundexInt(AValue: Integer): string;
Function SoundexWord(const AText: string): Word;
Function DecodeSoundexWord(AValue: Word): string;
Function SoundexSimilar(const AText, AOther: string; ALength: TSoundexLength): Boolean;
Function SoundexSimilar(const AText, AOther: string): Boolean; //; ALength: TSoundexLength = 4
Function SoundexCompare(const AText, AOther: string; ALength: TSoundexLength): Integer;
Function SoundexCompare(const AText, AOther: string): Integer; //; ALength: TSoundexLength = 4
Function SoundexProc(const AText, AOther: string): Boolean;

type
  TCompareTextProc = Function(const AText, AOther: string): Boolean;

Const
  AnsiResemblesProc: TCompareTextProc = @SoundexProc;

implementation

{ ---------------------------------------------------------------------
    Auxiliary functions
  ---------------------------------------------------------------------}

Procedure NotYetImplemented (FN : String);

begin
  Raise Exception.CreateFmt('Function "%s" (strutils) is not yet implemented',[FN]);
end;  

{ ---------------------------------------------------------------------
    Case sensitive search/replace
  ---------------------------------------------------------------------}

Function AnsiResemblesText(const AText, AOther: string): Boolean;

begin
  NotYetImplemented(' AnsiResemblesText');
end;

Function AnsiContainsText(const AText, ASubText: string): Boolean;

begin
  AnsiContainsText:=Pos(ASubText,AText)<>0;
end;

Function AnsiStartsText(const ASubText, AText: string): Boolean;
begin
  Result:=Copy(AText,1,Length(AsubText))=ASubText;
end;

Function AnsiEndsText(const ASubText, AText: string): Boolean;
begin
 result:=Copy(AText,Length(AText)-Length(ASubText)+1,Length(ASubText))=asubtext;
end;

Function AnsiReplaceText(const AText, AFromText, AToText: string): string;

var iFrom, iTo: longint;

begin
  iTo:=Pos(AFromText,AText);
  if iTo=0 then
    result:=AText
  else
    begin
     result:='';
     iFrom:=1;
     while (ito<>0) do
       begin
         result:=Result+Copy(AText,IFrom,Ito-IFrom+1)+AToText;
         ifrom:=ITo+Length(afromtext);
         ito:=Posex(Afromtext,atext,ifrom);
       end;
     if ifrom<=length(atext) then
      result:=result+copy(AText,ifrom, length(atext));
    end;
end;

Function AnsiMatchText(const AText: string; const AValues: array of string): Boolean;

var i : longint;

begin
  result:=false;
  if high(AValues)=-1 Then exit;
  for i:=low(AValues) to High(Avalues) do
     if avalues[i]=atext Then
       result:=true;
end;



Function AnsiIndexText(const AText: string; const AValues: array of string): Integer;

var i : longint;

begin
  result:=-1;
  if high(AValues)=-1 Then exit;
  for i:=low(AValues) to High(Avalues) do
     if avalues[i]=atext Then
       exit(i);					// make sure it is the first val.
end;


{ ---------------------------------------------------------------------
    Case insensitive search/replace
  ---------------------------------------------------------------------}

Function AnsiContainsStr(const AText, ASubText: string): Boolean;

begin
  NotYetImplemented(' AnsiContainsStr');
end;



Function AnsiStartsStr(const ASubText, AText: string): Boolean;

begin
  NotYetImplemented(' AnsiStartsStr');
end;



Function AnsiEndsStr(const ASubText, AText: string): Boolean;

begin
  NotYetImplemented(' AnsiEndsStr');
end;



Function AnsiReplaceStr(const AText, AFromText, AToText: string): string;

begin
  NotYetImplemented(' AnsiReplaceStr');
end;



Function AnsiMatchStr(const AText: string; const AValues: array of string): Boolean;

begin
  NotYetImplemented(' AnsiMatchStr');
end;



Function AnsiIndexStr(const AText: string; const AValues: array of string): Integer;

begin
  NotYetImplemented(' AnsiIndexStr');
end;




{ ---------------------------------------------------------------------
    Playthingies
  ---------------------------------------------------------------------}

Function DupeString(const AText: string; ACount: Integer): string;

var i,l : integer;

begin
 result:='';
 if aCount>=0 then
   begin
     l:=length(atext);
     SetLength(result,aCount*l);
     for i:=0 to ACount-1 do
       move(atext[1],Result[l*i+1],l);
   end;
end;

Function ReverseString(const AText: string): string;

var c: char;
    i,j:longint;

begin
  setlength(result,length(atext));
  i:=1; j:=length(atext);
  while (i<=j) do
    begin
      result[i]:=atext[j-i+1];
      inc(i);
    end;
end;


Function AnsiReverseString(const AText: AnsiString): AnsiString;

begin
  NotYetImplemented(' AnsiReverseString');
end;



Function StuffString(const AText: string; AStart, ALength: Cardinal;  const ASubText: string): string;

var i,j : longint;

begin
  j:=length(ASubText);
  i:=length(AText);
  SetLength(Result,i-ALength+j);
  move (AText[1],result[1],AStart-1);
  move (ASubText[1],result[AStart],j);
  move (AText[AStart+ALength], Result[AStart+j],i-AStart-ALength+1);
end;



Function RandomFrom(const AValues: array of string): string; overload;

begin
  if high(AValues)=-1 then exit('');
  result:=Avalues[random(High(AValues)+1)];
end;



Function IfThen(AValue: Boolean; const ATrue: string; AFalse: string): string; 

begin
  if avalue then result:=atrue else result:=afalse;
end;



Function IfThen(AValue: Boolean; const ATrue: string): string; // ; AFalse: string = ''

begin
  if avalue then result:=atrue else result:='';
end;



{ ---------------------------------------------------------------------
    VB emulations.
  ---------------------------------------------------------------------}

Function LeftStr(const AText: AnsiString; const ACount: Integer): AnsiString; 

begin
  Result:=Copy(AText,1,ACount);
end;

Function RightStr(const AText: AnsiString; const ACount: Integer): AnsiString; 

var j,l:integer;

begin
  l:=length(atext);
  j:=ACount;
  if j>l then j:=l;
  Result:=Copy(AText,l-j+1,j);
end;

Function MidStr(const AText: AnsiString; const AStart, ACount: Integer): AnsiString; 

begin
  if (ACount=0) or (AStart>length(atext)) then
    exit('');
  Result:=Copy(AText,AStart,ACount);  
end;



Function LeftBStr(const AText: AnsiString; const AByteCount: Integer): AnsiString;

begin
  NotYetImplemented(' LeftBStr');
end;



Function RightBStr(const AText: AnsiString; const AByteCount: Integer): AnsiString;

begin
  NotYetImplemented(' RightBStr');
end;



Function MidBStr(const AText: AnsiString; const AByteStart, AByteCount: Integer): AnsiString;

begin
  NotYetImplemented(' MidBStr');
end;



Function AnsiLeftStr(const AText: AnsiString; const ACount: Integer): AnsiString;

begin
  NotYetImplemented(' AnsiLeftStr');
end;



Function AnsiRightStr(const AText: AnsiString; const ACount: Integer): AnsiString;

begin
  NotYetImplemented(' AnsiRightStr');
end;



Function AnsiMidStr(const AText: AnsiString; const AStart, ACount: Integer): AnsiString;

begin
  NotYetImplemented(' AnsiMidStr');
end;

{$ifndef ver1_0}
Function LeftStr(const AText: WideString; const ACount: Integer): WideString; 

begin
  NotYetImplemented(' LeftStr');
end;



Function RightStr(const AText: WideString; const ACount: Integer): WideString; 

begin
  NotYetImplemented(' RightStr');
end;



Function MidStr(const AText: WideString; const AStart, ACount: Integer): WideString; 

begin
  NotYetImplemented(' MidStr');
end;
{$endif}




{ ---------------------------------------------------------------------
    Extended search and replace
  ---------------------------------------------------------------------}

Function SearchBuf(Buf: PChar; BufLen: Integer; SelStart, SelLength: Integer; SearchString: String; Options: TStringSearchOptions): PChar;

begin
  NotYetImplemented(' SearchBuf');
end;



Function SearchBuf(Buf: PChar; BufLen: Integer; SelStart, SelLength: Integer; SearchString: String): PChar; // ; Options: TStringSearchOptions = [soDown]

begin
  NotYetImplemented(' SearchBuf');
end;



Function PosEx(const SubStr, S: string; Offset: Cardinal): Integer;

var i : pchar;
begin
  if (offset<1) or (offset>length(s)) then exit(0);
  i:=strpos(@s[1],@substr[offset]);
  if i=nil then
    PosEx:=0   
  else
    PosEx:=(i-pchar(s))+offset;
end;


Function PosEx(const SubStr, S: string): Integer; // Offset: Cardinal = 1

begin
  posex:=posex(substr,s,1);
end;

Function PosEx(c:char; const S: string; Offset: Cardinal): Integer;

var l : longint;
begin
  if (offset<1) or (offset>length(s)) then exit(0);
  l:=length(s);
{$ifndef useindexbyte}
  while (offset<=l) and (s[offset]<>c) do inc(offset);
  if offset>l then
   posex:=0
  else 
   posex:=offset;
{$else}
  posex:=offset+indexbyte(s[offset],l-offset+1);
  if posex=(offset-1) then
    posex:=0;
{$endif}
end;


{ ---------------------------------------------------------------------
    Soundex Functions.
  ---------------------------------------------------------------------}
Const
SScore : array[1..255] of Char =
     ('0','0','0','0','0','0','0','0','0','0','0','0','0','0','0','0','0','0','0','0','0','0','0','0','0','0','0','0','0','0','0','0', // 1..32
      '0','0','0','0','0','0','0','0','0','0','0','0','0','0','0','0','0','0','0','0','0','0','0','0','0','0','0','0','0','0','0','0', // 33..64
      '0','1','2','3','0','1','2','i','0','2','2','4','5','5','0','1','2','6','2','3','0','1','i','2','i','2', // 64..90
      '0','0','0','0','0','0', // 91..95
      '0','1','2','3','0','1','2','i','0','2','2','4','5','5','0','1','2','6','2','3','0','1','i','2','i','2', // 96..122
      '0','0','0','0','0','0','0','0','0','0','0','0','0','0','0','0','0','0','0','0','0','0','0','0','0','0','0','0','0','0','0','0', // 123..154
      '0','0','0','0','0','0','0','0','0','0','0','0','0','0','0','0','0','0','0','0','0','0','0','0','0','0','0','0','0','0','0','0', // 155..186
      '0','0','0','0','0','0','0','0','0','0','0','0','0','0','0','0','0','0','0','0','0','0','0','0','0','0','0','0','0','0','0','0', // 187..218
      '0','0','0','0','0','0','0','0','0','0','0','0','0','0','0','0','0','0','0','0','0','0','0','0','0','0','0','0','0','0','0','0', // 219..250
      '0','0','0','0','0'); // 251..255 
                    


Function Soundex(const AText: string; ALength: TSoundexLength): string;

Var
  S,PS : Char;
  I,L : integer;
  
begin
  Result:='';
  PS:=#0;
  If Length(AText)>0 then
    begin
    Result:=Upcase(AText[1]);
    I:=2;
    L:=Length(AText);
    While (I<=L) and (Length(Result)<ALength) do
      begin
      S:=SScore[Ord(AText[i])];
      If Not (S in ['0','i',PS]) then
        Result:=Result+S;
      If (S<>'i') then
        PS:=S;
      Inc(I);  
      end;
    end;
  L:=Length(Result);
  If (L<ALength) then  
    Result:=Result+StringOfChar('0',Alength-L);
end;



Function Soundex(const AText: string): string; // ; ALength: TSoundexLength = 4

begin
  Result:=Soundex(AText,4);
end;



Function SoundexInt(const AText: string; ALength: TSoundexIntLength): Integer;

begin
  NotYetImplemented(' SoundexInt');
end;



Function SoundexInt(const AText: string): Integer; //; ALength: TSoundexIntLength = 4

begin
  NotYetImplemented(' SoundexInt');
end;



Function DecodeSoundexInt(AValue: Integer): string;

begin
  NotYetImplemented(' DecodeSoundexInt');
end;



Function SoundexWord(const AText: string): Word;

Var  
  S : String;

begin
  S:=SoundEx(Atext,4);
  Writeln('Soundex result : "',S,'"');
  Result:=Ord(S[1])-Ord('A');
  Result:=Result*26+StrToInt(S[2]);
  Result:=Result*7+StrToInt(S[3]);
  Result:=Result*7+StrToInt(S[4]);
end;



Function DecodeSoundexWord(AValue: Word): string;

begin
  NotYetImplemented(' DecodeSoundexWord');
end;



Function SoundexSimilar(const AText, AOther: string; ALength: TSoundexLength): Boolean;

begin
  NotYetImplemented(' SoundexSimilar');
end;



Function SoundexSimilar(const AText, AOther: string): Boolean; //; ALength: TSoundexLength = 4

begin
  NotYetImplemented(' SoundexSimilar');
end;



Function SoundexCompare(const AText, AOther: string; ALength: TSoundexLength): Integer;

begin
  NotYetImplemented(' SoundexCompare');
end;



Function SoundexCompare(const AText, AOther: string): Integer; //; ALength: TSoundexLength = 4

begin
  NotYetImplemented(' SoundexCompare');
end;



Function SoundexProc(const AText, AOther: string): Boolean;

begin
  NotYetImplemented(' SoundexProc');
end;

end.

{
  $Log$
  Revision 1.4  2004-03-19 12:54:22  marco
   * more strutils small things

  Revision 1.3  2004/03/18 16:55:47  marco
   * more simple implementations done, based on copy() Largely untested

}