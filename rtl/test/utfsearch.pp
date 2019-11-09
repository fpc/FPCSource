{
    This file is part of the Free Pascal test suite.
    Copyright (c) 1999-2003 by the Free Pascal development team.

    Test for possible bugs in Dos.FSearch

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}
{$mode objfpc}

unit utfsearch;

interface

uses punit, utrtl;

implementation

uses
 Dos;

const
  TestDir: string = 'TESTDIR';
  TestFile: string = 'testfile';
  {$IFDEF MACOS}
  RelPathPrefix = ':';
  {$ELSE}
  RelPathPrefix = '';
  {$ENDIF}

Function DoTestFSearch : TTestString;

var
  R,S: string;
  F: file;

begin
  Result:='';
  S := FSearch (TestDir, '');
  If not AssertEquals('FSearch should only find files, not directories!!','',S) then exit;
  // Create test file
  Assign (F, RelPathPrefix + TestDir + DirectorySeparator + TestFile);
  Rewrite (F);
  Close (F);
  S:=FSearch (TestFile, TestDir);
  // expected result
  R:=RelPathPrefix + TestDir + DirectorySeparator + TestFile;
  If not AssertEquals('FSearch didn''t find the test file!!',R,S) then exit;
end;

Function TestFSearch : TTestString;

var
  F: file;

begin
  MkDir (TestDir);
  Result:=DoTestFSearch;
  // Clean up
{$i-}
  Assign (F, RelPathPrefix + TestDir + DirectorySeparator + TestFile);
  Erase (F);
  RmDir (TestDir);
{$i+}
end;

begin
  AddTest('TestFSearch',@TestFsearch,EnsureSuite('Dos'));
end.
