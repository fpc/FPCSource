{$mode objfpc}
{$h+}
{
    $Id$
    This file is part of the Free Component Library (FCL)
    Copyright (c) 2004 by Dean Zobec, Michael Van Canneyt
    
    Port to Free Pascal of the JUnit framework. 

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}
unit testregistry;

interface

uses
  fpcunit;


procedure RegisterTest(ATestClass: TTestClass); overload;

procedure RegisterTests(ATests: Array of TTestClass);

function NumberOfRegisteredTests: longint;

function GetTestRegistry: TTestSuite;

implementation

var
  FTestRegistry: TTestSuite;

function GetTestRegistry: TTestSuite;
begin
  if not Assigned(FTestRegistry) then
    FTestRegistry := TTestSuite.Create;
  Result := FTestRegistry;
end;

procedure RegisterTest(ATestClass: TTestClass);
begin
  GetTestRegistry.AddTestSuiteFromClass(ATestClass);
end;

procedure RegisterTests(ATests: Array of TTestClass);
var
  i: integer;
begin
  for i := Low(ATests) to High(ATests) do
    if Assigned(ATests[i]) then
    begin
      RegisterTest(ATests[i]);
    end;
end;


function NumberOfRegisteredTests: longint;
begin
  Result := GetTestRegistry.CountTestCases;
end;

initialization
  FTestRegistry := nil;
finalization
  FTestRegistry.Free;
end.

