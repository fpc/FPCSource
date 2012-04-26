{
    This file is part of the Free Component Library (FCL)
    Copyright (c) 2012 by the Free Pascal development team

    HTML text reader
    
    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}
unit IReaderHTML;

{$mode objfpc}{$H+}

interface

uses
  FastHTMLParser, //, HTMLUtil,          // Fast Parser Functions
  Classes, fpIndexer;

type

  { TIReaderHTML }

  TIReaderHTML = class(TCustomFileReader)
  private
    sLine: string;
    StartPos: integer;
    Offset: integer;
    LinePos: integer;
    Tg, Tx: integer;
    FParser: THTMLParser; //our htmlparser class
    procedure OnTag(NoCaseTag, ActualTag: string);
    procedure OnText(Text: string);
  protected
    function GetToken: string; override;
    function AllowedToken(token: string): boolean; override;
  public
    procedure LoadFromStream(FileStream: TStream); override;
  end;

implementation

{ TIReaderHTML }

procedure TIReaderHTML.OnTag(NoCaseTag, ActualTag: string);
begin
end;

procedure TIReaderHTML.OnText(Text: string);
var
  token: string;
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

function TIReaderHTML.GetToken: string;
var
  s: string;
  c: string;
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

function TIReaderHTML.AllowedToken(token: string): boolean;
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

