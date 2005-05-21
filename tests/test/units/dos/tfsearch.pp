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

program TFSearch;

uses
 Dos;

var
 S: string;
 F: file;
 Err: boolean;

const
 TestDir: string = 'TESTDIR';
 TestFile: string = 'testfile';

{$IFDEF MACOS}
 RelPathPrefix = ':';
{$ELSE}
 RelPathPrefix = '';
{$ENDIF}

begin
 Err := false;
 MkDir (TestDir);
 S := FSearch (TestDir, '');
 if S <> '' then
 begin
  WriteLn ('FSearch should only find files, not directories!!');
  WriteLn ('Returned value = ', S);
  Err := true;
 end;
 Assign (F, RelPathPrefix + TestDir + DirectorySeparator + TestFile);
 Rewrite (F);
 Close (F);
 S := FSearch (TestFile, TestDir);
 if S <> RelPathPrefix + TestDir + DirectorySeparator + TestFile then
 begin
  WriteLn ('FSearch didn''t find the test file!!');
  WriteLn ('Returned value = ', S);
  Err := true;
 end;
 Erase (F);
 RmDir (TestDir);
 if Err then Halt (1);
end.
