{$mode objfpc}
{$h+}
{
    This file is part of the Free Component Library (FCL)
    Copyright (c) 2005 by Uberto Barbini

    Ultra basic implementation of mock objects for endo-testing
    with fpcunit.

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}
unit ubmockobject;

interface

uses
  Classes, SysUtils, fpcunit;

type

  TUbMockObject = class(TObject)
  protected
    FSetUpMode: Boolean;
    FSetUpList: TStringList;
    FCallsList: TStringList;
  public
    constructor Create; virtual;
    destructor Destroy; override;
    procedure AddExpectation(const ASignatureCall: string);
    property SetUpMode: Boolean read FSetUpMode;
    procedure Verify;
    procedure StartSetUp;
    procedure EndSetUp;
    function UncoveredExpectations: integer;
  end;

implementation

{ TUbMockObject }

procedure TUbMockObject.AddExpectation(const ASignatureCall: string);
begin
  if SetUpMode then
    FSetUpList.Add(ASignatureCall)
  else
    FCallsList.Add(ASignatureCall);
end;

function TUbMockObject.UncoveredExpectations: integer;
begin
  Result := FSetUpList.Count - FCallsList.Count;
end;

constructor TUbMockObject.Create;
begin
  FSetUpList := TStringList.Create;
  FCallsList := TStringList.Create;
  FSetUpMode := True;
end;

destructor TUbMockObject.Destroy;
begin
  FSetUpList.Free;
  FCallsList.Free;
  inherited;
end;

procedure TUbMockObject.EndSetUp;
begin
  FSetUpMode := False;
  FCallsList.Clear;
end;

procedure TUbMockObject.StartSetUp;
begin
  FSetUpMode := True;
  FSetUpList.Clear;
end;

procedure TUbMockObject.Verify;
var
  i: integer;
  s1, s2: string;
begin
  TAssert.AssertEquals('Wrong number of expectations!', FSetUpList.Count, FCallsList.Count);
  for i := 0 to FSetUpList.Count - 1 do
  begin
    s1 := FSetUpList[i];
    s2 := FCallsList[i];
    TAssert.AssertEquals(s1, s2);
  end;
  FSetUpList.Clear;
  FCallsList.Clear;
end;

end.
