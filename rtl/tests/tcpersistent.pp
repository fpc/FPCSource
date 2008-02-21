unit tcpersistent;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, fpcunit, testregistry;

type

  { TTestTPersistent }

  TTestTPersistent= class(TTestCase)
  protected
    Instance : TPersistent;
    procedure SetUp; override; 
    procedure TearDown; override; 
  published
    procedure TestPropCount;
    procedure TestNamePath;
  end; 
  
  { TMyPersistent }

  TMyPersistent = Class(TPersistent)
  private
    FMyProp: Integer;
    FOwner : TPersistent;
  protected
    function GetOwner: TPersistent; override;
  public
    procedure Assign(Source: TPersistent); override;
  published
    Property MyProp : Integer Read FMyProp Write FMyProp;
  end;

  { TTestPersistentDescendent }

  TTestPersistentDescendent = class(TTestCase)
  private
    procedure WrongAssign;
  Protected
    Instance : TMyPersistent;
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure TestPropCount;
    procedure TestNamePath;
    procedure TestNamePathWithOwner;
    Procedure TestAssign;
    Procedure TestAssignFail;
  end;


implementation

uses typinfo;

procedure TTestTPersistent.TestPropCount;

Var
  ACOunt : Integer;
  P : Pointer;
  
begin
  P:=Nil;
  ACOunt:=GetPropList(Instance,P);
  AssertEquals('Property count of TPersistence is zero',0,ACount);
end;

procedure TTestTPersistent.TestNamePath;
begin
  AssertEquals('Namepath is class name if there is no owner','TPersistent',Instance.GetNamePath);
end;

procedure TTestTPersistent.SetUp; 
begin
  Instance:=TPersistent.Create;
end;

procedure TTestTPersistent.TearDown; 
begin
  FreeAndNil(Instance);
end; 

{ TTestPersistentDescendent }

procedure TTestPersistentDescendent.SetUp;
begin
  Instance:=TMyPersistent.Create;
end;

procedure TTestPersistentDescendent.TearDown;
begin
  FreeAndNil(Instance);
end;

procedure TTestPersistentDescendent.TestPropCount;

Var
  ACOunt : Integer;
  P : Pointer;

begin
  P:=Nil;
  ACount:=GetPropList(Instance,P);
  AssertEquals('Property count of TPersistence is zero',1,ACount);
end;

procedure TTestPersistentDescendent.TestNamePath;
begin
  AssertEquals('Namepath is class name if there is no owner','TMyPersistent',Instance.GetNamePath);
end;

procedure TTestPersistentDescendent.TestNamePathWithOwner;

Var
  AOwner : TMyPersistent;
  
begin
  AOwner:=TMyPersistent.Create;
  try
    Instance.FOwner:=AOwner;
    AssertEquals('Namepath is owner namepath plus class name','TMyPersistent.TMyPersistent',Instance.GetNamePath);
  finally
    Aowner.Free;
  end;
end;

procedure TTestPersistentDescendent.TestAssign;

Var
  I2 : TMyPersistent;
  
begin
  I2:=TMyPersistent.Create;
  try
    I2.MyProp:=2;
    Instance.Assign(I2);
    AssertEquals('Property passed on during assign',2,Instance.MyProp);
  finally
    I2.Free;
  end;
end;


procedure TTestPersistentDescendent.TestAssignFail;

begin
  AssertException('Assigning the wrong class',EConvertError,@WrongAssign);
end;

procedure TTestPersistentDescendent.WrongAssign;
Var
  I2 : TPersistent;

begin
  I2:=TPersistent.Create;
  try
    Instance.Assign(I2);
  finally
    I2.Free;
  end;
end;

{ TMyPersistent }

function TMyPersistent.GetOwner: TPersistent;
begin
  Result:=FOwner;
end;

procedure TMyPersistent.Assign(Source: TPersistent);
begin
  If (Source is TMyPersistent) then
    FMyProp:=TMyPersistent(Source).FMyProp
  else
    Inherited;
end;

initialization

  RegisterTests([TTestTPersistent,TTestPersistentDescendent]);
end.

