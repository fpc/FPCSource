{
    This file is part of the Free Component Library (Fcl)
    Copyright (c) 2012 by the Free Pascal development team

    HTML text reader
    
    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}
{$IFNDEF FPC_DOTTEDUNITS}
unit IReaderHTML;
{$ENDIF FPC_DOTTEDUNITS}

{$mode objfpc}{$H+}

interface

{$IFDEF FPC_DOTTEDUNITS}
uses
  Fcl.FastHtmlParser, //, Fcl.Htmlutil,          // Fast Parser Functions
  System.Classes, FpIndexer.Indexer;
{$ELSE FPC_DOTTEDUNITS}
uses
  FastHTMLParser, //, HTMLUtil,          // Fast Parser Functions
  Classes, fpIndexer;
{$ENDIF FPC_DOTTEDUNITS}

type

  { TIReaderHTML }

  TIReaderHTML = class(TCustomFileReader)
  private
    sLine: UTF8String;
    StartPos: integer;
    Offset: integer;
    LinePos: integer;
    Tg, Tx: integer;
    FParser: THTMLParser; //our htmlparser class
    procedure OnTag(NoCaseTag, ActualTag: AnsiString);
    procedure OnText(Text: AnsiString);
  protected
    function GetToken: UTF8String; override;
    function AllowedToken(token: UTF8String): boolean; override;
  public
    procedure LoadFromStream(FileStream: TStream); override;
  end;

implementation

{ TIReaderHTML }

procedure TIReaderHTML.OnTag(NoCaseTag, ActualTag: AnsiString);
begin
end;

procedure TIReaderHTML.OnText(Text: AnsiString);
var
  token: UTF8String;
  s: TSearchWordData;
  i : Integer;
begin
  sLine := Text;
  LinePos := 1;
  Offset:=FParser.CurrentPos;
  token := GetToken;
  while token <> '' do
    begin
    if AllowedToken(token) then
      begin
      s.SearchWord := token;
      s.Position := Offset+StartPos;
      // Copy area around text.
      I:=StartPos-(MaxContextLen div 2);
      If I<1 then
        I:=1;
      s.Context := Copy(SLine,I,I+MaxContextLen);
      Add(s);
      end;
    token := GetToken;
    end;
end;

function TIReaderHTML.GetToken: UTF8String;
var
  s: UTF8String;
  c: UTF8String;
begin
  Result := '';

  if (sLine = '') or (LinePos >= Length(sLine)) then
    exit;

  c := sLine[LinePos];
  Inc(LinePos);

  if LinePos <= Length(sLine) then
  begin

    //eat all invalid characters
    while not (c[1] in ['a'..'z', 'A'..'Z', '0'..'9']) and (LinePos <= Length(sLine)) do
    begin
      c := sLine[LinePos];
      Inc(LinePos);
    end;

    if not (c[1] in ['a'..'z', 'A'..'Z', '0'..'9']) then
      s := ''
    else
      s := c;
    StartPos:=LinePos;
    if LinePos <= Length(sLine) then
    begin
      //now read all valid characters from stream and append
      c := sLine[LinePos];
      Inc(LinePos);
      while (c[1] in ['a'..'z', 'A'..'Z', '0'..'9']) and (LinePos <= Length(sLine)) do
      begin
        s := S + c;
        c := sLine[LinePos];
        Inc(LinePos);
      end;
    end;

    if not (c[1] in ['a'..'z', 'A'..'Z', '0'..'9']) then
      Result := LowerCase(s)
    else
      Result := LowerCase(s + c);
  end;
end;

function TIReaderHTML.AllowedToken(token: UTF8String): boolean;
begin
  Result := (Length(token) > 1) and
            (token <> 'nbsp') and (token <> 'quot') and (token <> 'apos') and
            (token <> 'amp') and (token <> 'lt') and (token <> 'gt');
end;

procedure TIReaderHTML.LoadFromStream(FileStream: TStream);
var
  S : TStringStream;

begin
  inherited LoadFromStream(FileStream);
  S:=TStringStream.Create('');
  try
    S.CopyFrom(FileStream,0);
    Tg := 0;
    Tx := 0;
    FParser := THTMLParser.Create(S.DataString);
    try
      FParser.OnFoundTag := @OnTag;
      FParser.OnFoundText := @OnText;
      FParser.Exec;
    finally
      FParser.Free;
     end;
  finally
    S.Free;
  end;
  if DetectLanguage then
    DoDetectLanguage;
end;

initialization
  FileHandlers.RegisterFileReader('HTML format', 'html;htm', TIReaderHTML);

end.

