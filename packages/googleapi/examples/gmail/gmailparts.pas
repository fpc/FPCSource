{ Helpers for Gmail API messages: headers, encoded words, text body,
  attachments and base64url data. }
unit GmailParts;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, gmail.Dto;

type
  TMessagePartList = array of TMessagePart;

// Return the value of header aName (case-insensitive) of a message part, or '' if absent.
function HeaderValue(aPart: TMessagePart; const aName: string): string;
// Decode base64url data as used by the Gmail API (padding optional).
function DecodeBase64URL(const aData: string): RawByteString;
// Decode RFC 2047 encoded words (=?charset?B|Q?text?=) in a header value to UTF-8.
function DecodeHeaderWords(const aValue: string): string;
// Return the charset parameter of a Content-Type value, lowercase, or '' if absent.
function ContentTypeCharset(const aContentType: string): string;
// Convert text in charset aCharset to UTF-8; ISO-8859-1 and Windows-1252 are converted, others returned unchanged.
function CharsetToUTF8(const aText: RawByteString; const aCharset: string): string;
// Return the first non-attachment part with MIME type aMimeType, searching depth first, or nil.
function FindTextPart(aPart: TMessagePart; const aMimeType: string): TMessagePart;
// Append the parts that have a file name (the attachments) to aList, depth first.
procedure CollectAttachments(aPart: TMessagePart; var aList: TMessagePartList);
// Return the text of a message: the text/plain part, or the text/html part without markup.
function MessageText(aMessage: TMessage): string;
// Remove HTML markup and decode common character entities.
function StripHTML(const aHTML: string): string;
// Convert a Gmail internalDate (milliseconds since 1970-01-01 UTC) to local time.
function InternalDateToLocal(const aInternalDate: string): TDateTime;

implementation

uses
  base64, DateUtils, StrUtils;

function HeaderValue(aPart: TMessagePart; const aName: string): string;

var
  lHeader: TMessagePartHeader;

begin
  Result:='';
  if aPart=nil then
    exit;
  for lHeader in aPart.headers do
    if SameText(lHeader.name,aName) then
      Exit(lHeader.value);
end;


function DecodeBase64URL(const aData: string): RawByteString;

var
  lData: string;

begin
  lData:=StringReplace(aData,'-','+',[rfReplaceAll]);
  lData:=StringReplace(lData,'_','/',[rfReplaceAll]);
  while (Length(lData) mod 4)<>0 do
    lData:=lData+'=';
  Result:=DecodeStringBase64(lData);
end;


// Encode one byte (0..255) of ISO-8859-1 text as UTF-8.
function Latin1CharToUTF8(aChar: AnsiChar): string;

var
  lCode: Byte;

begin
  lCode:=Ord(aChar);
  if lCode<$80 then
    Result:=aChar
  else
    Result:=Chr($C0 or (lCode shr 6))+Chr($80 or (lCode and $3F));
end;


function CharsetToUTF8(const aText: RawByteString; const aCharset: string): string;

var
  lI: Integer;

begin
  if (aCharset='iso-8859-1') or (aCharset='latin1') or (aCharset='windows-1252') or (aCharset='us-ascii') then
    begin
    Result:='';
    for lI:=1 to Length(aText) do
      Result:=Result+Latin1CharToUTF8(aText[lI]);
    end
  else
    Result:=aText;
  SetCodePage(RawByteString(Result),CP_UTF8,False);
end;


// Decode the text of a Q-encoded word: '_' is a space, =XX is a hexadecimal byte.
function DecodeQ(const aText: string): RawByteString;

var
  lI: Integer;

begin
  Result:='';
  lI:=1;
  while lI<=Length(aText) do
    begin
    if aText[lI]='_' then
      Result:=Result+' '
    else if (aText[lI]='=') and (lI+2<=Length(aText)) then
      begin
      Result:=Result+Chr(StrToIntDef('$'+Copy(aText,lI+1,2),Ord('?')));
      Inc(lI,2);
      end
    else
      Result:=Result+aText[lI];
    Inc(lI);
    end;
end;


function DecodeHeaderWords(const aValue: string): string;

var
  lPos, lStart, lQ1, lQ2, lEnd: Integer;
  lCharset, lEncoding, lText, lGap: string;
  lDecoded: RawByteString;
  lPrevEncoded: Boolean;

begin
  Result:='';
  lPrevEncoded:=False;
  lPos:=1;
  while lPos<=Length(aValue) do
    begin
    lStart:=PosEx('=?',aValue,lPos);
    if lStart=0 then
      break;
    lQ1:=PosEx('?',aValue,lStart+2);
    lQ2:=0;
    lEnd:=0;
    if lQ1>0 then
      lQ2:=PosEx('?',aValue,lQ1+1);
    if lQ2=lQ1+2 then
      lEnd:=PosEx('?=',aValue,lQ2+1);
    if lEnd=0 then
      break;
    lCharset:=LowerCase(Copy(aValue,lStart+2,lQ1-lStart-2));
    lEncoding:=UpperCase(Copy(aValue,lQ1+1,1));
    lText:=Copy(aValue,lQ2+1,lEnd-lQ2-1);
    if lEncoding='B' then
      lDecoded:=DecodeStringBase64(lText)
    else
      lDecoded:=DecodeQ(lText);
    // Whitespace between two encoded words is dropped
    lGap:=Copy(aValue,lPos,lStart-lPos);
    if not (lPrevEncoded and (Trim(lGap)='')) then
      Result:=Result+lGap;
    Result:=Result+CharsetToUTF8(lDecoded,lCharset);
    lPrevEncoded:=True;
    lPos:=lEnd+2;
    end;
  Result:=Result+Copy(aValue,lPos,Length(aValue)-lPos+1);
end;


function ContentTypeCharset(const aContentType: string): string;

var
  lPos, lEnd: Integer;

begin
  Result:='';
  lPos:=Pos('charset=',LowerCase(aContentType));
  if lPos=0 then
    exit;
  Result:=Copy(aContentType,lPos+8,Length(aContentType));
  lEnd:=Pos(';',Result);
  if lEnd>0 then
    Result:=Copy(Result,1,lEnd-1);
  Result:=LowerCase(Trim(StringReplace(Result,'"','',[rfReplaceAll])));
end;


function FindTextPart(aPart: TMessagePart; const aMimeType: string): TMessagePart;

var
  lSub: TMessagePart;

begin
  Result:=nil;
  if aPart=nil then
    exit;
  if SameText(aPart.mimeType,aMimeType) and (aPart.filename='') then
    Exit(aPart);
  for lSub in aPart.parts do
    begin
    Result:=FindTextPart(lSub,aMimeType);
    if Result<>nil then
      exit;
    end;
end;


procedure CollectAttachments(aPart: TMessagePart; var aList: TMessagePartList);

var
  lSub: TMessagePart;

begin
  if aPart=nil then
    exit;
  if aPart.filename<>'' then
    begin
    SetLength(aList,Length(aList)+1);
    aList[High(aList)]:=aPart;
    end;
  for lSub in aPart.parts do
    CollectAttachments(lSub,aList);
end;


// Return the decoded text of a part, converted to UTF-8.
function PartText(aPart: TMessagePart): string;

begin
  Result:='';
  if Assigned(aPart.body) and (aPart.body.data<>'') then
    Result:=CharsetToUTF8(DecodeBase64URL(aPart.body.data),
                          ContentTypeCharset(HeaderValue(aPart,'Content-Type')));
end;


function MessageText(aMessage: TMessage): string;

var
  lPart: TMessagePart;

begin
  Result:='';
  lPart:=FindTextPart(aMessage.payload,'text/plain');
  if lPart<>nil then
    Exit(PartText(lPart));
  lPart:=FindTextPart(aMessage.payload,'text/html');
  if lPart<>nil then
    Result:=StripHTML(PartText(lPart));
end;


// Remove all elements aTag including their content, case-insensitive.
function RemoveElement(const aHTML, aTag: string): string;

var
  lStart, lEnd: Integer;
  lLower: string;

begin
  Result:=aHTML;
  repeat
    lLower:=LowerCase(Result);
    lStart:=Pos('<'+aTag,lLower);
    if lStart=0 then
      break;
    lEnd:=PosEx('</'+aTag+'>',lLower,lStart);
    if lEnd=0 then
      lEnd:=Length(Result)+1
    else
      lEnd:=lEnd+Length(aTag)+3;
    Delete(Result,lStart,lEnd-lStart);
  until False;
end;


function StripHTML(const aHTML: string): string;

const
  Entities: array[0..6,0..1] of string = (
    ('&nbsp;',' '), ('&lt;','<'), ('&gt;','>'), ('&quot;','"'),
    ('&#39;',''''), ('&apos;',''''), ('&amp;','&'));

var
  lText, lTag: string;
  lI, lEnd: Integer;

begin
  lText:=RemoveElement(RemoveElement(aHTML,'style'),'script');
  Result:='';
  lI:=1;
  while lI<=Length(lText) do
    begin
    if lText[lI]='<' then
      begin
      lEnd:=PosEx('>',lText,lI);
      if lEnd=0 then
        lEnd:=Length(lText);
      lTag:=LowerCase(Copy(lText,lI+1,lEnd-lI-1));
      if (Copy(lTag,1,2)='br') or (Copy(lTag,1,2)='/p') or (Copy(lTag,1,4)='/div')
         or (Copy(lTag,1,3)='/tr') or (Copy(lTag,1,3)='/li') or (Copy(lTag,1,3)='/h1') then
        Result:=Result+LineEnding;
      lI:=lEnd+1;
      continue;
      end;
    if lText[lI] in [#13,#10] then
      Result:=Result+' '
    else
      Result:=Result+lText[lI];
    Inc(lI);
    end;
  for lI:=Low(Entities) to High(Entities) do
    Result:=StringReplace(Result,Entities[lI,0],Entities[lI,1],[rfReplaceAll,rfIgnoreCase]);
  while Pos(LineEnding+LineEnding+LineEnding,Result)>0 do
    Result:=StringReplace(Result,LineEnding+LineEnding+LineEnding,LineEnding+LineEnding,[rfReplaceAll]);
  Result:=Trim(Result);
end;


function InternalDateToLocal(const aInternalDate: string): TDateTime;

var
  lMilliSeconds: Int64;

begin
  lMilliSeconds:=StrToInt64Def(aInternalDate,0);
  if lMilliSeconds=0 then
    Exit(0);
  Result:=UniversalTimeToLocal(UnixToDateTime(lMilliSeconds div 1000));
end;


end.
