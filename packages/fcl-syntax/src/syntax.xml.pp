{
    This file is part of the Free Component Library (FCL)
    Copyright (c) 2026 by Michael Van Canneyt

    XML syntax highlighter

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}

{$MODE objfpc}
{$H+}

unit syntax.xml;

interface

uses
  {$IFDEF FPC_DOTTEDUNITS}
  System.Types, syntax.highlighter, syntax.table;
  {$ELSE}
  Types, syntax.highlighter, syntax.table;
  {$ENDIF}

type

  { TXmlSyntaxHighlighter }

  TXmlSyntaxHighlighter = class(TByteSyntaxHighlighter)
  protected
    class function GetLanguages: TStringDynArray; override;
    // Split aSource in tokens
    procedure DoExecute(const aSource: string); override;
  end;

implementation

class function TXmlSyntaxHighlighter.GetLanguages: TStringDynArray;

begin
  Result := ['xml', 'svg', 'xsd', 'lpi', 'lpk', 'lps'];
end;


procedure TXmlSyntaxHighlighter.DoExecute(const aSource: string);

var
  lFirst, lBrackets: SizeInt;
  lInTag, lTagName: boolean;
  lQuote: char;
  lKind: TSyntaxHighlightKind;
begin
  Start(aSource);
  lInTag := False;
  lTagName := False;
  while FCursor <= FLimit do
    begin
    lFirst := FCursor;
    lKind := shDefault;
    if At('<!--') then
      begin
      Block('<!--', '-->', False);
      lKind := shComment;
      end
    else if At('<![CDATA[') then
      begin
      Quoted('<![CDATA[', ']]>', #0, True, False);
      lKind := shRawString;
      end
    else if At('<?') then
      begin
      Quoted('<?', '?>', #0, True, False);
      lKind := shDirective;
      end
    else if At('<!') then
      begin
      Inc(FCursor, 2);
      lBrackets := 0;
      while FCursor <= FLimit do
        begin
        if Ch in ['"', ''''] then
          begin
          lQuote := Ch;
          Quoted(lQuote, lQuote, #0, True, False);
          end
        else if At('<!--') then
          Block('<!--', '-->', False)
        else
          begin
          if Ch = '[' then
            Inc(lBrackets);
          if (Ch = ']') and (lBrackets > 0) then
            Dec(lBrackets);
          if (Ch = '>') and (lBrackets = 0) then
            begin
            Inc(FCursor);
            Break;
            end;
          Inc(FCursor);
          end;
        end;
      lKind := shDirective;
      end
    else if Ch = '<' then
      begin
      Inc(FCursor);
      if Ch = '/' then
        Inc(FCursor);
      lInTag := True;
      lTagName := True;
      lKind := shSymbol;
      end
    else if lInTag and (Ch in ['"', '''']) then
      begin
      lQuote := Ch;
      Quoted(lQuote, lQuote, #0, True, False);
      lKind := shStrings;
      end
    else if Ch = '&' then
      begin
      Inc(FCursor);
      while (FCursor <= FLimit) and (Ch in NameChars + ['#', 'x', '-', '.']) do
        Inc(FCursor);
      if Ch = ';' then
        Inc(FCursor);
      lKind := shEscape;
      end
    else if lInTag and (Ch in NameStartChars + [':']) then
      begin
      Inc(FCursor);
      while (FCursor <= FLimit) and (Ch in NameChars + [':', '-', '.']) do
        Inc(FCursor);
      if lTagName then
        lKind := shKeyword
      else
        lKind := shAttribute;
      lTagName := False;
      end
    else if lInTag and (Ch in ['/', '>', '=']) then
      begin
      if Ch = '>' then
        lInTag := False;
      Inc(FCursor);
      lKind := shSymbol;
      end
    else if lInTag then
      begin
      if Ch in [' ', #9, #10, #13] then
        White
      else
        Unknown;
      end
    else
      begin
      Inc(FCursor);
      while (FCursor <= FLimit) and not (Ch in ['<', '&']) do
        Inc(FCursor);
      end;
    Emit(lFirst, lKind);
    end;
  Finish;
end;

initialization
  TXmlSyntaxHighlighter.Register;
end.
