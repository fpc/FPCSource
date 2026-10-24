{ string properties survive ObjectBinaryToText and ObjectTextToBinary, issue #34046 }
program tobjtextstr;

{$mode objfpc}{$h+}

uses
{$ifdef unix}
  cwstring,
{$endif}
  Classes;

type
  TStrHolder = class(TComponent)
  private
    FText: string;
    FUText: UnicodeString;
  published
    // Ansi string property
    property Text: string read FText write FText;
    // Unicode string property
    property UText: UnicodeString read FUText write FUText;
  end;

var
  lCode: Integer;

// Halt with the next code and write aMsg when aOk is false
procedure Check(aOk: Boolean; const aMsg: string);

begin
  Inc(lCode);
  if not aOk then
    begin
    writeln('check ',lCode,' failed: ',aMsg);
    halt(lCode);
    end;
end;


// Return the component aSrc after a binary to text to binary round trip
function RoundTrip(aSrc: TStrHolder; aEncoding: TObjectTextEncoding): TStrHolder;

var
  lBin, lText, lBin2: TMemoryStream;

begin
  lBin:=TMemoryStream.Create;
  lText:=TMemoryStream.Create;
  lBin2:=TMemoryStream.Create;
  try
    lBin.WriteComponent(aSrc);
    lBin.Position:=0;
    ObjectBinaryToText(lBin,lText,aEncoding);
    lText.Position:=0;
    ObjectTextToBinary(lText,lBin2);
    lBin2.Position:=0;
    Result:=TStrHolder.Create(nil);
    lBin2.ReadComponent(Result);
  finally
    lBin.Free;
    lText.Free;
    lBin2.Free;
  end;
end;


// Round trip aValue through aEncoding with aCodePage as default system code page
procedure CheckValue(aCodePage: TSystemCodePage; aEncoding: TObjectTextEncoding; const aValue: UnicodeString; const aMsg: string);

var
  lSrc, lDst: TStrHolder;
  lAnsi: string;

begin
  DefaultSystemCodePage:=aCodePage;
  lSrc:=TStrHolder.Create(nil);
  try
    lAnsi:=string(aValue);
    lSrc.Text:=lAnsi;
    lSrc.UText:=aValue;
    lDst:=RoundTrip(lSrc,aEncoding);
    try
      Check(lDst.Text=lAnsi,aMsg+' ansi property');
      Check(lDst.UText=aValue,aMsg+' unicode property');
    finally
      lDst.Free;
    end;
  finally
    lSrc.Free;
  end;
end;


// Return the text of aText converted to binary and back
function TextRoundTrip(const aText: string): string;

var
  lText, lBin, lText2: TStringStream;

begin
  lText:=TStringStream.Create(aText);
  lBin:=TStringStream.Create('');
  lText2:=TStringStream.Create('');
  try
    ObjectTextToBinary(lText,lBin);
    lBin.Position:=0;
    ObjectBinaryToText(lBin,lText2);
    Result:=lText2.DataString;
  finally
    lText.Free;
    lBin.Free;
    lText2.Free;
  end;
end;


// Check a unit qualified class name of more than 255 characters
procedure CheckLongClassName;

var
  lUnitName, lResult: string;

begin
  lUnitName:='U'+StringOfChar('x',300);
  lResult:=TextRoundTrip('object Obj1: '+lUnitName+'/TStrHolder'+LineEnding+'end'+LineEnding);
  Check(Pos(lUnitName+'/TStrHolder',lResult)>0,'long unit qualified class name');
end;


const
  cLatin: UnicodeString = 'Gr'#$00FC#$00DF'e '#$00E9't'#$00E9;
  cMixed: UnicodeString = 'Gr'#$00FC#$00DF'e '#$0416#$03A9' '#$20AC' it''s';

var
  lLong: UnicodeString;

begin
  lCode:=0;
  lLong:=StringOfChar(WideChar('a'),300)+cMixed;
  CheckValue(CP_UTF8,oteDFM,'plain ascii','ascii, utf-8, dfm');
  CheckValue(CP_UTF8,oteDFM,cMixed,'non-ascii, utf-8, dfm');
  CheckValue(CP_UTF8,oteDFM,lLong,'long non-ascii, utf-8, dfm');
  CheckValue(CP_UTF8,oteLFM,cMixed,'non-ascii, utf-8, lfm');
  CheckValue(CP_UTF8,oteLFM,lLong,'long non-ascii, utf-8, lfm');
  CheckValue(1252,oteDFM,cLatin,'latin, cp1252, dfm');
  CheckValue(1252,oteDFM,StringOfChar(WideChar('b'),300)+cLatin,'long latin, cp1252, dfm');
  CheckLongClassName;
  writeln('ok');
end.
