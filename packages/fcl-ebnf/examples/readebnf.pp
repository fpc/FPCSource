{
    This file is part of the Free Component Library (FCL)
    Copyright (c) 2025 by Michael Van Canneyt (michael@freepascal.org)

    EBNF grammar Parser demo

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}
{$mode objfpc}
{$h+}
program readebnf;

{$IFDEF WINDOWS}
{$APPTYPE CONSOLE}
{$ENDIF}

uses
  SysUtils, classes,
  ebnf.tree,
  ebnf.parser;

var
  EBNFSource: string;
  Parser: TEBNFParser;
  Grammar: TEBNFGrammar;
  Rule: TEBNFRule;
  List : TStrings;

begin
  // Our example
  if ParamCount=1 then
    EBNFSource:=GetFileAsString(ParamStr(1))
  else
    begin
    Writeln('Using example source. Provide filename to parse actual file.');
    Writeln('');
    EBNFSource :=
      'program = statement { ";" statement } ;' + sLineBreak +
      'statement = "IF" expression "THEN" statement [ "ELSE" statement ]' + sLineBreak +
      '          | identifier "=" expression' + sLineBreak +
      '          | "PRINT" ( string_literal | identifier ) ;' + sLineBreak +
      'expression = term { ("+" | "-") term } ;' + sLineBreak +
      'term = factor { ("*" | "/") factor } ;' + sLineBreak +
      'factor = identifier | string_literal | number | "(" expression ")" | "?comment?" ;';
    end;

  Parser := nil;
  Grammar := nil;
  try
    Parser := TEBNFParser.Create(EBNFSource);
    Grammar := Parser.Parse;

    Writeln('Successfully parsed EBNF grammar:');
    Writeln('-----------------------------------');
    Writeln(Grammar.ToString);
    Writeln('-----------------------------------');

    // demo accessing a specific rule
    Rule:=Grammar.Rules['program'];
    if Assigned(Rule)  then
    begin
      Writeln('Details for rule "program":');
      Writeln(Rule.ToString);
    end;
    List:=TStringList.Create;
    try
      Grammar.FindUndefinedIdentifiers(List);
      if List.Count>0 then
        begin
        Writeln('Undefined meta-identifiers:');
        Writeln(list.text);
        end;
    finally
      list.Free;
    end;

  except
    on E: Exception do
      Writeln('Error: ' + E.Message);
  end;
  Grammar.Free;
  Parser.Free;
end.
