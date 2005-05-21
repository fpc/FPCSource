{$mode objfpc}
{$h+}
{
    This file is part of the Free Component Library (FCL)
    Copyright (c) 2005 Uberto Barbini

    mock object tests

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}
unit testmockobject;

interface

uses
  Classes, SysUtils, fpcunit, testutils, testregistry, ubmockobject;

type

  TTestMock= class(TTestCase)
  protected
    _MockObj: TUbMockObject;
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure TestRightCalls;
    procedure TestWrongCallCount;
    procedure TestWrongOrderOfCalls;
    procedure TestWrongSignatures;
    procedure TestExpectations;
    procedure TestResetExpectations;
  end;

implementation

{ TTestMock }

procedure TTestMock.SetUp;
begin
  _MockObj := TUbMockObject.Create;
end;

procedure TTestMock.TearDown;
begin
  _MockObj.Free;
end;

procedure TTestMock.TestRightCalls;
begin
  _MockObj.StartSetUp;
  _MockObj.AddExpectation('abc');
  _MockObj.AddExpectation('123');
  _MockObj.AddExpectation('The quick brown fox jumped over the lazy dog.');
  AssertEquals(3, _MockObj.UncoveredExpectations);
  _MockObj.EndSetUp;
  _MockObj.AddExpectation('abc');
  _MockObj.AddExpectation('123');
  _MockObj.AddExpectation('The quick brown fox jumped over the lazy dog.');
  _MockObj.Verify;
  AssertEquals(0, _MockObj.UncoveredExpectations);
end;

procedure TTestMock.TestWrongCallCount;
begin
  _MockObj.StartSetUp;
  _MockObj.EndSetUp;
  _MockObj.AddExpectation('abc');
  AssertException(EAssertionFailedError, @_MockObj.Verify);
  AssertEquals(-1, _MockObj.UncoveredExpectations);

  _MockObj.StartSetUp;
  _MockObj.AddExpectation('abc');
  _MockObj.EndSetUp;
  AssertException(EAssertionFailedError, @_MockObj.Verify);
  AssertEquals(1, _MockObj.UncoveredExpectations);
end;

procedure TTestMock.TestWrongSignatures;
begin
  _MockObj.StartSetUp;
  _MockObj.AddExpectation('aaa');
  _MockObj.EndSetUp;
  _MockObj.AddExpectation('bbb');
  AssertException(EAssertionFailedError, @_MockObj.Verify);
end;

procedure TTestMock.TestWrongOrderOfCalls;
begin
  _MockObj.StartSetUp;
  _MockObj.AddExpectation('abc');
  _MockObj.AddExpectation('123');
  _MockObj.AddExpectation('the quick brown fox jumped over the lazy dog');
  _MockObj.EndSetUp;
  _MockObj.AddExpectation('abc');
  _MockObj.AddExpectation('the quick brown fox jumped over the lazy dog');
  _MockObj.AddExpectation('123');
  AssertException(EAssertionFailedError, @_MockObj.Verify);
end;

procedure TTestMock.TestExpectations;
begin
  AssertTrue(_MockObj.SetUpMode);
  _MockObj.StartSetUp;
  _MockObj.EndSetUp;
  AssertTrue(not _MockObj.SetUpMode);
  _MockObj.Verify;
end;

procedure TTestMock.TestResetExpectations;
begin
  _MockObj.AddExpectation('abc');
  _MockObj.StartSetUp;
  _MockObj.AddExpectation('123');
  _MockObj.EndSetUp;
  _MockObj.AddExpectation('123');
  _MockObj.Verify;
end;

initialization
  RegisterTests([TTestMock]);
end.
