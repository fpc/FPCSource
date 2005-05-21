{$mode objfpc}
{$h+}
{
    This file is part of the Free Component Library (FCL)
    Copyright (c) 2004 by Dean Zobec

    Port to Free Pascal of the JUnit framework.

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}
unit asserttest;

interface

uses
  fpcunit, testregistry;

type

  TAssertTest = class(TTestCase)
  published
    procedure TestFail;
    procedure TestAssertSame;
    procedure TestAssertSameNull;
    procedure TestAssertNotSameFailsNull;
    procedure TestAssertStringEquals;
    procedure TestNullNotSameObject;
    procedure TestAssertNull;
    procedure TestAssertNotNull;
    procedure TestAssertTrue;
    procedure TestAssertFalse;
    procedure TestAssertNotSame;
  end;

implementation

procedure TAssertTest.TestFail;
begin
  try
    fail('Wrong or no exception raised with fail');
  except
    on E: EAssertionfailedError do
      Exit;
  end;
  raise EAssertionFailedError.Create;
end;

procedure TAssertTest.TestAssertSame;
var
  o: TObject;
  o1: TObject;
begin
  o := TObject.Create;
  AssertSame(o, o);
  o1 := TObject.Create;
  try
    AssertSame(o, o1);
  except
    on E: EAssertionFailedError do
    begin
      o.Free;
      o1.Free;
      Exit;
    end;
  end;
  o.Free;
  o1.Free;
  Fail('Wrong or no exception raised');
end;

procedure TAssertTest.TestAssertSameNull;
var
  a, b: TObject;
begin
  a := nil;
  b := nil;
  AssertSame(a, b);
  AssertSame(nil, a);
  AssertSame(a, nil);
end;

procedure TAssertTest.TestAssertNotSameFailsNull;
var
  a, b: TObject;
begin
  a := nil;
  b := nil;
  try
    assertNotSame(a, b);
  except
    on E: EAssertionFailedError do
    Exit;
  end;
  fail('error: nil should equal nil');
end;

procedure TAssertTest.TestAssertStringEquals;
begin
  AssertEquals('a', 'a')
end;

procedure TAssertTest.TestNullNotSameObject;
var
  obj: TObject;
begin
  obj := TObject.Create;
  try
    AssertSame(nil, obj);
  except
    on E: EAssertionFailedError do
    begin
      obj.Free;
      Exit;
    end;
  end;
  Fail('error comparing a valid obj instance with nil');
end;

procedure TAssertTest.TestAssertNull;
var
  obj: TObject;
begin
  AssertNull(nil);
  obj := TObject.Create;
  try
    AssertNull(obj);
  except
    on E: EAssertionFailedError do
    begin
      obj.Free;
      Exit;
    end;
  end;
  obj.Free;
  Fail('failure: obj is not null!');
end;

procedure TAssertTest.TestAssertNotNull;
var
  obj: TObject;
begin
  obj := TObject.Create;
  AssertNotNull(obj);
  try
    AssertNotNull(nil);
  except
    on E: EAssertionFailedError do
    begin
      obj.Free;
      Exit;
    end;
  end;
  obj.Free;
  Fail('error: nil is not a valid object');
end;

procedure TAssertTest.TestAssertTrue;
begin
  assertTrue(true);
  try
    assertTrue(false);
  except
    on E: EAssertionFailedError do
    Exit;
  end;
  fail('error asserting true');
end;

procedure TAssertTest.TestAssertFalse;
begin
  assertFalse(false);
  try
    assertFalse(true);
  except
    on E: EAssertionFailedError do
    Exit;
  end;
  fail('error asserting false');
end;

procedure TAssertTest.TestAssertNotSame;
var
  obj: TObject;
  obj1: TObject;
begin
  obj := TObject.Create;
  obj1 := TObject.Create;
  AssertNotSame(obj, nil);
  AssertNotSame(nil, obj);
  AssertNotSame(obj, obj1);
  try
    AssertNotSame(obj, obj)
  except
    on E: EAssertionFailedError do
    begin
      obj.Free;
      obj1.Free;
      Exit;
    end;
  end;
  obj.Free;
  obj1.Free;
  Fail('Error: Objects are the same!');
end;

initialization

  RegisterTests([TAssertTest]);

end.
