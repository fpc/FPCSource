{ Source provided for Free Pascal bug report 19610 }
{ Submitted by Reinier Olislagers on 20120920 }
{
Note: the tests here are somewhat more extensive than the original bug report.
They are aimed at confirming interoperability between Delphi and FPC sdf formats
The basis for the tests is therefore Delphi's handling.

The only exception are the Put tests, which also accept results that are always
quoted. As Get_StrictDelimTrueSafeQuote and Get_StrictDelimFalseSafeQuote prove,
always quoting output leads to correct/the same input.
The advantage of this is that having strictdelimiter on or off does not matter
and the output format is more unambiguous (i.e. more compatible with RFC4180
for CSV).

On Delphi, rename to .dpr.

Tests successfully completed on:
Turbo Delphi 2006 (Reinier Olislagers)
Delphi 2007 (OBones)
Delphi XE (Marco van de Voort, OBones)
Delphi XE2 Win32 (OBones)
Delphi XE2 Win64 (OBones)
}
{
    This file is part of the Free Pascal packages.
    Copyright (c) 1999-2012 by the Free Pascal development team

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}
{$apptype console}
{$ifdef fpc}
  {$mode objfpc}{$H+}
{$endif fpc}
program tw19610;

uses Classes, SysUtils;

{$ifndef fpc}
//Delphi
const
  LineEnding=#13+#10;
{$endif}

function Get_StrictDelimFalse:boolean;
// Test if input works with Delphi-compatible sdf output
// Strictdelimiter:=false (default) when processing the delimitedtext
//
// Mainly check if reading quotes is according to Delphi sdf specs and works.
// Based on del4.zip in bug 19610
const
  // Matches del4.zip in bug 19610:
  DelimText='normal_string;"quoted_string";"quoted;delimiter";"quoted and space";"""quoted_and_starting_quote";"""quoted, starting quote, and space";"quoted_with_tab'+#9+'character";"quoted_multi'+LineEnding+
    'line";  UnquotedSpacesInfront;UnquotedSpacesAtTheEnd   ;  "Spaces before quoted string"';
  TestName='tw19610.Get_StrictDelimFalse';
var
  TestSL: TStringList;
  Expected: TStringList;
  i: integer;
begin
  result:=true;
  //Expected values:
  Expected:=TStringList.Create;
  TestSL:=TStringList.Create;
  try
    Expected.Add('normal_string');
    Expected.Add('quoted_string');
    Expected.Add('quoted;delimiter');
    Expected.Add('quoted and space');
    Expected.Add('"quoted_and_starting_quote');
    Expected.Add('"quoted, starting quote, and space');
    Expected.Add('quoted_with_tab'+#9+'character');
    Expected.Add('quoted_multi'+LineEnding+
      'line');
    Expected.Add('UnquotedSpacesInfront');
    Expected.Add('UnquotedSpacesAtTheEnd');
    Expected.Add('Spaces before quoted string');

    TestSL.Delimiter:=';'; //Match example in bug 19610, del4.zip
    TestSL.StrictDelimiter:=false;
    TestSL.DelimitedText:=DelimText;
    //Test:
    if Expected.Count<>TestSL.Count then
    begin
      writeln('');
      writeln(TestName+': failed: count mismatch: '+
      inttostr(TestSL.Count)+' test strings; '+inttostr(Expected.Count)+' expected strings.');
    end;

    for i:=0 to TestSL.Count-1 do
    begin
      if (Expected.Count>i) and (TestSL[i]<>Expected[i]) then
      begin
        writeln('');
        writeln(TestName+': failed: result:');
        writeln('*'+TestSL[i]+'*');
        writeln('while expected was:');
        writeln('*'+Expected[i]+'*');
        result:=false;
      end;
    end;
  finally
    Expected.Free;
    TestSL.Free;
  end;
end;

function Get_StrictDelimTrue:boolean;
// Test if input works with Delphi-compatible sdf output
// Strictdelimiter:=true when processing the delimitedtext
//
// Mainly check if reading quotes is according to Delphi sdf specs and works.
// Based on del4.zip in bug 19610
const
  // Matches del4.zip in bug 19610:
  DelimText='normal_string;"quoted_string";"quoted;delimiter";"quoted and space";"""quoted_and_starting_quote";"""quoted, starting quote, and space";"quoted_with_tab'+#9+'character";"quoted_multi'+LineEnding+
    'line";  UnquotedSpacesInfront;UnquotedSpacesAtTheEnd   ;  "Spaces before quoted string"';
  TestName='tw19610.Get_StrictDelimTrue';
var
  TestSL: TStringList;
  Expected: TStringList;
  i: integer;
begin
  result:=true;
  //Expected values:
  Expected:=TStringList.Create;
  TestSL:=TStringList.Create;
  try
    Expected.Add('normal_string');
    Expected.Add('quoted_string');
    Expected.Add('quoted;delimiter');
    Expected.Add('quoted and space');
    Expected.Add('"quoted_and_starting_quote');
    Expected.Add('"quoted, starting quote, and space');
    Expected.Add('quoted_with_tab'+#9+'character');
    Expected.Add('quoted_multi'+LineEnding+
      'line');
    Expected.Add('  UnquotedSpacesInfront');
    Expected.Add('UnquotedSpacesAtTheEnd   ');
    Expected.Add('  "Spaces before quoted string"');

    TestSL.Delimiter:=';'; //Match example in bug 19610, del4.zip
    TestSL.StrictDelimiter:=true;
    TestSL.DelimitedText:=DelimText;
    //Test:
    if Expected.Count<>TestSL.Count then
    begin
      writeln('');
      writeln(TestName+': failed: count mismatch: '+
      inttostr(TestSL.Count)+' test strings; '+inttostr(Expected.Count)+' expected strings.');
    end;
    for i:=0 to TestSL.Count-1 do
    begin
      if (Expected.Count>i) and (TestSL[i]<>Expected[i]) then
      begin
        writeln('');
        writeln(TestName+': failed: result:');
        writeln('*'+TestSL[i]+'*');
        writeln('while expected was:');
        writeln('*'+Expected[i]+'*');
        result:=false;
      end;
    end;
  finally
    Expected.Free;
    TestSL.Free;
  end;
end;

function Get_StrictDelimFalseCornerCases:boolean;
// Test if input works with Delphi-compatible sdf output
// Strictdelimiter:=false (default) when processing the delimitedtext
//
// Has some corner cases that Delphi produces but are not evident from their
// documentation
// Based on del4.zip in bug 19610
const
  // Matches del4.zip in bug 19610:
  DelimText='"Spaces after quoted string"   ;';
  TestName='tw19610.Get_StrictDelimFalseCornerCases';
var
  TestSL: TStringList;
  Expected: TStringList;
  i: integer;
begin
  result:=true;
  //Expected values:
  Expected:=TStringList.Create;
  TestSL:=TStringList.Create;
  try
    Expected.Add('Spaces after quoted string');
    Expected.Add('');

    TestSL.Delimiter:=';'; //Match example in bug 19610, del4.zip
    TestSL.StrictDelimiter:=false;
    TestSL.DelimitedText:=DelimText;
    //Test:
    if Expected.Count<>TestSL.Count then
    begin
      writeln('');
      writeln(TestName+': failed: count mismatch: '+
      inttostr(TestSL.Count)+' test strings; '+inttostr(Expected.Count)+' expected strings.');
    end;
    for i:=0 to TestSL.Count-1 do
    begin
      if (Expected.Count>i) and (TestSL[i]<>Expected[i]) then
      begin
        writeln('');
        writeln(TestName+': failed: result:');
        writeln('*'+TestSL[i]+'*');
        writeln('while expected was:');
        writeln('*'+Expected[i]+'*');
        result:=false;
      end;
    end;
  finally
    Expected.Free;
    TestSL.Free;
  end;
end;

function Get_StrictDelimTrueCornerCases:boolean;
// Test if input works with Delphi-compatible sdf output
// Strictdelimiter:=true when processing the delimitedtext
//
// Has some corner cases that Delphi produces but are not evident from their
// documentation
// Based on del4.zip in bug 19610
const
  // Matches del4.zip in bug 19610:
  DelimText='"Spaces after quoted string"   ;';
  TestName='tw19610.Get_StrictDelimTrueCornerCases';
var
  TestSL: TStringList;
  Expected: TStringList;
  i: integer;
begin
  result:=true;
  //Expected values:
  Expected:=TStringList.Create;
  TestSL:=TStringList.Create;
  try
    // With delimiter true, we get 2 extra empty lines, also some spaces
    Expected.Add('Spaces after quoted string');
    Expected.Add('   ');
    Expected.Add('');

    TestSL.Delimiter:=';'; //Match example in bug 19610, del4.zip
    TestSL.StrictDelimiter:=true;
    TestSL.DelimitedText:=DelimText;
    //Test:
    if Expected.Count<>TestSL.Count then
    begin
      writeln('');
      writeln(TestName+': failed: count mismatch: '+
      inttostr(TestSL.Count)+' test strings; '+inttostr(Expected.Count)+' expected strings.');
    end;
    for i:=0 to TestSL.Count-1 do
    begin
      if (Expected.Count>i) and (TestSL[i]<>Expected[i]) then
      begin
        writeln('');
        writeln(TestName+': failed: result:');
        writeln('*'+TestSL[i]+'*');
        writeln('while expected was:');
        writeln('*'+Expected[i]+'*');
        result:=false;
      end;
    end;
  finally
    Expected.Free;
    TestSL.Free;
  end;
end;

function Get_StrictDelimTrueSafeQuote:boolean;
// Test if input works with sdf output that has always been quoted
// Delphi accepts this input even though it does not write it by default
// This is a more unambiguous format than unquoted
// Strictdelimiter:=true when processing the delimitedtext
//
const
  DelimText='"normal_string";"""quoted_string""";"""quoted;delimiter""";"""quoted and space""";"""starting_quote";"string_with_tab'+#9+'character";"multi'+LineEnding+
    'line";"  SpacesInfront";"SpacesAtTheEnd   ";"  ""Spaces before quoted string"""';
  TestName='tw19610.Get_StrictDelimTrueSafeQuote';
var
  TestSL: TStringList;
  Expected: TStringList;
  i: integer;
begin
  result:=true;
  //Expected values:
  Expected:=TStringList.Create;
  TestSL:=TStringList.Create;
  try
    Expected.Add('normal_string');
    Expected.Add('"quoted_string"');
    Expected.Add('"quoted;delimiter"');
    Expected.Add('"quoted and space"');
    Expected.Add('"starting_quote');
    Expected.Add('string_with_tab'+#9+'character');
    Expected.Add('multi'+LineEnding+
      'line');
    Expected.Add('  SpacesInfront');
    Expected.Add('SpacesAtTheEnd   ');
    Expected.Add('  "Spaces before quoted string"');

    TestSL.Delimiter:=';'; //Match example in bug 19610, del4.zip
    TestSL.StrictDelimiter:=true;
    TestSL.DelimitedText:=DelimText;
    //Test:
    if Expected.Count<>TestSL.Count then
    begin
      writeln('');
      writeln(TestName+': failed: count mismatch: '+
      inttostr(TestSL.Count)+' test strings; '+inttostr(Expected.Count)+' expected strings.');
    end;
    for i:=0 to TestSL.Count-1 do
    begin
      if (Expected.Count>i) and (TestSL[i]<>Expected[i]) then
      begin
        writeln('');
        writeln(TestName+': failed: result:');
        writeln('*'+TestSL[i]+'*');
        writeln('while expected was:');
        writeln('*'+Expected[i]+'*');
        result:=false;
      end;
    end;
  finally
    Expected.Free;
    TestSL.Free;
  end;
end;

function Get_StrictDelimFalseSafeQuote:boolean;
// Test if input works with sdf output that has always been quoted
// Delphi accepts this input even though it does not write it by default
// This is a more unambiguous format than unquoted
// Strictdelimiter:=false when processing the delimitedtext
//
const
  DelimText='"normal_string";"""quoted_string""";"""quoted;delimiter""";"""quoted and space""";"""starting_quote";"string_with_tab'+#9+'character";"multi'+LineEnding+
    'line";"  SpacesInfront";"SpacesAtTheEnd   ";"  ""Spaces before quoted string"""';
  TestName='tw19610.Get_StrictDelimTrueSafeQuote';
var
  TestSL: TStringList;
  Expected: TStringList;
  i: integer;
begin
  result:=true;
  //Expected values:
  Expected:=TStringList.Create;
  TestSL:=TStringList.Create;
  try
    Expected.Add('normal_string');
    Expected.Add('"quoted_string"');
    Expected.Add('"quoted;delimiter"');
    Expected.Add('"quoted and space"');
    Expected.Add('"starting_quote');
    Expected.Add('string_with_tab'+#9+'character');
    Expected.Add('multi'+LineEnding+
      'line');
    Expected.Add('  SpacesInfront');
    Expected.Add('SpacesAtTheEnd   ');
    Expected.Add('  "Spaces before quoted string"');

    TestSL.Delimiter:=';'; //Match example in bug 19610, del4.zip
    TestSL.StrictDelimiter:=false;
    TestSL.DelimitedText:=DelimText;
    //Test:
    if Expected.Count<>TestSL.Count then
    begin
      writeln('');
      writeln(TestName+': failed: count mismatch: '+
      inttostr(TestSL.Count)+' test strings; '+inttostr(Expected.Count)+' expected strings.');
    end;
    for i:=0 to TestSL.Count-1 do
    begin
      if (Expected.Count>i) and (TestSL[i]<>Expected[i]) then
      begin
        writeln('');
        writeln(TestName+': failed: result:');
        writeln('*'+TestSL[i]+'*');
        writeln('while expected was:');
        writeln('*'+Expected[i]+'*');
        result:=false;
      end;
    end;
  finally
    Expected.Free;
    TestSL.Free;
  end;
end;

function Get_Commatext:boolean;
// Test if input works with Delphi-compatible commatext
const
  CommaText='normal_string,"quoted_string","quoted,delimiter","quoted and space","""quoted_and_starting_quote","""quoted, starting quote, and space","quoted_with_tab'+#9+'character","quoted_multi'+LineEnding+
    'line","  UnquotedSpacesInfront","UnquotedSpacesAtTheEnd   ","  ""Spaces before quoted string"""';
  TestName='tw19610.Get_Commatext';
var
  TestSL: TStringList;
  Expected: TStringList;
  i: integer;
begin
  result:=true;
  //Expected values:
  Expected:=TStringList.Create;
  TestSL:=TStringList.Create;
  try
    Expected.Add('normal_string');
    Expected.Add('quoted_string');
    Expected.Add('quoted,delimiter');
    Expected.Add('quoted and space');
    Expected.Add('"quoted_and_starting_quote');
    Expected.Add('"quoted, starting quote, and space');
    Expected.Add('quoted_with_tab'+#9+'character');
    Expected.Add('quoted_multi'+LineEnding+
      'line');
    Expected.Add('  UnquotedSpacesInfront');
    Expected.Add('UnquotedSpacesAtTheEnd   ');
    Expected.Add('  "Spaces before quoted string"');

    TestSL.CommaText:=CommaText;
    //Test:
    if Expected.Count<>TestSL.Count then
    begin
      writeln('');
      writeln(TestName+': failed: count mismatch: '+
      inttostr(TestSL.Count)+' test strings; '+inttostr(Expected.Count)+' expected strings.');
    end;

    for i:=0 to TestSL.Count-1 do
    begin
      if (Expected.Count>i) and (TestSL[i]<>Expected[i]) then
      begin
        writeln('');
        writeln(TestName+': failed: result:');
        writeln('*'+TestSL[i]+'*');
        writeln('while expected was:');
        writeln('*'+Expected[i]+'*');
        result:=false;
      end;
    end;
  finally
    Expected.Free;
    TestSL.Free;
  end;
end;


function Put_StrictDelimFalse:boolean;
// Test if conversion stringlist=>delimitedtext gives the right data
// (right in this case: what Delphi outputs)
// Strictdelimiter:=false when processing the delimitedtext
const
  Expected='normal_string;"""quoted_string""";"just;delimiter";"""quoted;delimiter""";"""quoted and space""";"""starting_quote";"single""quote";"""""quoted starting quote and space""";"with_tab'+#9+'character";"multi'+LineEnding+
    'line";"   UnquotedSpacesInfront";"UnquotedSpacesAtTheEnd  ";"  ""Spaces before quoted string"""';
  //If we choose to output the "safely quoted" version, we need to test for it:
  //Though this version is not the same output as Delphi, it leads to the
  //same input if imported again (see Get_StrictDelimFalseSafeQuote for corresponding tests)
  ExpectedSafeQuote='"normal_string";"""quoted_string""";"just;delimiter";"""quoted;delimiter""";"""quoted and space""";"""starting_quote";"single""quote";"""""quoted starting quote and space""";"with_tab'+#9+'character";"multi'+LineEnding+
    'line";"   UnquotedSpacesInfront";"UnquotedSpacesAtTheEnd  ";"  ""Spaces before quoted string"""';
  TestName='tw19610.Put_StrictDelimFalse';
var
  TestSL: TStringList;
begin
  result:=true;
  TestSL:=TStringList.Create;
  try
    TestSL.Add('normal_string');
    TestSL.Add('"quoted_string"');
    TestSL.Add('just;delimiter');
    TestSL.Add('"quoted;delimiter"');
    TestSL.Add('"quoted and space"');
    TestSL.Add('"starting_quote');
    TestSL.Add('single"quote');
    TestSL.Add('""quoted starting quote and space"');
    TestSL.Add('with_tab'+#9+'character');
    TestSL.Add('multi'+LineEnding+
      'line');
    TestSL.Add('   UnquotedSpacesInfront');
    TestSL.Add('UnquotedSpacesAtTheEnd  ');
    TestSL.Add('  "Spaces before quoted string"');

    TestSL.Delimiter:=';';
    TestSL.StrictDelimiter:=false;
    if (TestSL.DelimitedText<>Expected) and (TestSL.DelimitedText<>ExpectedSafeQuote) then
    begin
      writeln('');
      writeln(TestName+': failed: result:');
      writeln('*'+TestSL.DelimitedText+'*');
      writeln('while expected was:');
      writeln('*'+Expected+'*');
      writeln('- or, with safe quote output:');
      writeln('*'+ExpectedSafeQuote+'*');
      result:=false
    end;
  finally
    TestSL.Free;
  end;
end;

function Put_StrictDelimTrue:boolean;
// Test if conversion stringlist=>delimitedtext gives the right data
// (right in this case: what Delphi outputs)
// Strictdelimiter:=true when processing the delimitedtext
const
  Expected='normal_string;"""quoted_string""";"just;delimiter";"""quoted;delimiter""";"""quoted and space""";"""starting_quote";"single""quote";"""""quoted starting quote and space""";with_tab'+#9+'character;multi'+LineEnding+
    'line;   UnquotedSpacesInfront;UnquotedSpacesAtTheEnd  ;"  ""Spaces before quoted string"""';
  //If we choose to output the "safely quoted" version, we need to test for it:
  //Though this version is not the same output as Delphi, it leads to the
  //same input if imported again (see Get_StrictDelimTrueSafeQuote for corresponding tests)
  ExpectedSafeQuote='"normal_string";"""quoted_string""";"just;delimiter";"""quoted;delimiter""";"""quoted and space""";"""starting_quote";"single""quote";"""""quoted starting quote and space""";"with_tab'+#9+'character";"multi'+LineEnding+
    'line";"   UnquotedSpacesInfront";"UnquotedSpacesAtTheEnd  ";"  ""Spaces before quoted string"""';
  TestName='tw19610.Put_StrictDelimTrue';
var
  TestSL: TStringList;
begin
  result:=true;
  TestSL:=TStringList.Create;
  try
    TestSL.Add('normal_string');
    TestSL.Add('"quoted_string"');
    TestSL.Add('just;delimiter');
    TestSL.Add('"quoted;delimiter"');
    TestSL.Add('"quoted and space"');
    TestSL.Add('"starting_quote');
    TestSL.Add('single"quote');
    TestSL.Add('""quoted starting quote and space"');
    TestSL.Add('with_tab'+#9+'character');
    TestSL.Add('multi'+LineEnding+
      'line');
    TestSL.Add('   UnquotedSpacesInfront');
    TestSL.Add('UnquotedSpacesAtTheEnd  ');
    TestSL.Add('  "Spaces before quoted string"');

    TestSL.Delimiter:=';';
    TestSL.StrictDelimiter:=true;
    if (TestSL.DelimitedText<>Expected) and (TestSL.DelimitedText<>ExpectedSafeQuote) then
    begin
      writeln('');
      writeln(TestName+': failed: result:');
      writeln('*'+TestSL.DelimitedText+'*');
      writeln('while expected was:');
      writeln('*'+Expected+'*');
      writeln('- or, with safe quote output:');
      writeln('*'+ExpectedSafeQuote+'*');
      result:=false
    end;
  finally
    TestSL.Free;
  end;
end;

function GetPut_StrictDelimFalse:boolean;
// Test if conversion stringlist=>delimitedtext=>stringlist gives identical data
// Strictdelimiter:=false (default) when processing the delimitedtext
const
  TestName='tw19610.GetPut_StrictDelimFalse';
var
  TestSL: TStringList;
  ResultSL: TStringList;
  i: integer;
begin
  result:=true;
  ResultSL:=TStringList.Create;
  TestSL:=TStringList.Create;
  try
    TestSL.Add('normal_string');
    TestSL.Add('"quoted_string"');
    TestSL.Add('"quoted;delimiter"');
    TestSL.Add('"quoted and space"');
    TestSL.Add('"starting_quote');
    TestSL.Add('""quoted, starting quote, and space"');
    TestSL.Add('with_tab'+#9+'character');
    TestSL.Add('multi'+LineEnding+
      'line');
    TestSL.Add('   UnquotedSpacesInfront');
    TestSL.Add('UnquotedSpacesAtTheEnd  ');
    TestSL.Add('  "Spaces before quoted string"');

    TestSL.Delimiter:=';';
    TestSL.StrictDelimiter:=false;
    ResultSL.Delimiter:=';';
    ResultSL.StrictDelimiter:=false;
    ResultSL.DelimitedText:=TestSL.DelimitedText;
    //Test:
    if ResultSL.Count<>TestSL.Count then
    begin
      writeln('');
      writeln(TestName+': failed: count mismatch: '+
      inttostr(TestSL.Count)+' test strings; '+inttostr(ResultSL.Count)+' expected strings.');
    end;

    for i:=0 to TestSL.Count-1 do
    begin
      if (ResultSL.Count>i) and (TestSL[i]<>ResultSL[i]) then
      begin
        writeln('');
        writeln(TestName+': failed: result:');
        writeln('*'+TestSL[i]+'*');
        writeln('while expected was:');
        writeln('*'+ResultSL[i]+'*');
        result:=false;
      end;
    end;
  finally
    ResultSL.Free;
    TestSL.Free;
  end;
end;

function GetPut_StrictDelimTrue:boolean;
// Test if conversion stringlist=>delimitedtext=>stringlist gives identical data
// Strictdelimiter:=true when processing the delimitedtext
const
  TestName='tw19610.GetPut_StrictDelimTrue';
var
  TestSL: TStringList;
  ResultSL: TStringList;
  i: integer;
begin
  result:=true;
  ResultSL:=TStringList.Create;
  TestSL:=TStringList.Create;
  try
    TestSL.Add('normal_string');
    TestSL.Add('"quoted_string"');
    TestSL.Add('"quoted;delimiter"');
    TestSL.Add('"quoted and space"');
    TestSL.Add('"starting_quote');
    TestSL.Add('""quoted, starting quote, and space"');
    TestSL.Add('with_tab'+#9+'character');
    TestSL.Add('multi'+LineEnding+
      'line');
    TestSL.Add('   UnquotedSpacesInfront');
    TestSL.Add('UnquotedSpacesAtTheEnd  ');
    TestSL.Add('  "Spaces before quoted string"');

    TestSL.Delimiter:=';';
    TestSL.StrictDelimiter:=false;
    ResultSL.Delimiter:=';';
    ResultSL.StrictDelimiter:=true;
    ResultSL.DelimitedText:=TestSL.DelimitedText;
    //Test:
    if ResultSL.Count<>TestSL.Count then
    begin
      writeln('');
      writeln(TestName+': failed: count mismatch: '+
      inttostr(TestSL.Count)+' test strings; '+inttostr(ResultSL.Count)+' expected strings.');
    end;

    for i:=0 to TestSL.Count-1 do
    begin
      if (ResultSL.Count>i) and (TestSL[i]<>ResultSL[i]) then
      begin
        writeln('');
        writeln(TestName+': failed: result:');
        writeln('*'+TestSL[i]+'*');
        writeln('while expected was:');
        writeln('*'+ResultSL[i]+'*');
        result:=false;
      end;
    end;
  finally
    ResultSL.Free;
    TestSL.Free;
  end;
end;

var
  FailCount: integer;
begin
  FailCount:=0;
  // The Get_... tests load in delimitedtext and test the resulting stringlist:
  if not(Get_StrictDelimFalse) then FailCount:=FailCount+1;
  if not(Get_StrictDelimTrue) then FailCount:=FailCount+1;
  if not(Get_StrictDelimFalseCornerCases) then FailCount:=FailCount+1;
  if not(Get_StrictDelimTrueCornerCases) then FailCount:=FailCount+1;
  if not(Get_StrictDelimTrueSafeQuote) then FailCount:=FailCount+1;
  if not(Get_StrictDelimFalseSafeQuote) then FailCount:=FailCount+1;

  if not(Get_CommaText) then FailCount:=FailCount+1;

  // The Put_... tests load strings and test the resulting delimitedtext:
  if not(Put_StrictDelimFalse) then FailCount:=FailCount+1;
  if not(Put_StrictDelimTrue) then FailCount:=FailCount+1;

  // Test writing to delimitedtext and reading from delimitedtext:
  if not(GetPut_StrictDelimFalse) then FailCount:=FailCount+1;
  if not(GetPut_StrictDelimTrue) then FailCount:=FailCount+1;

  // Indicate success or failure to test framework:
  if FailCount=0 then
  begin
    writeln('');
    writeln('tw19610: sdf tests succeeded.');
  end
  else
  begin
    writeln('');
    writeln('tw19610: sdf test(s) failed. Number of failed test group(s): '+inttostr(FailCount));
  end;

  halt(FailCount);
end.
