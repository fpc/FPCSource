unit tcfindnested;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, fpcunit, testutils, testregistry; 

type

  { TTestFindComponent }

  TTestFindComponent= class(TTestCase)
  Private
    R,A,B,AC,BC,D : TComponent;
    Function CreateNamed(AOwner : TComponent; AName : String) : TComponent;
    Procedure CheckFind(Root : TComponent; AName : String; Expected : TComponent);
  Protected
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure TestFindA;
    procedure TestEmpty;
    procedure TestFindB;
    procedure TestFindACaseDiffer;
    procedure TestFindBCaseDiffer;
    procedure TestFindNonExist;
    procedure TestFindNonExistSub;
    procedure TestFindOwner;
    procedure TestFindOwnerNameOwner;
    procedure TestFindOwnerNamed;
    procedure TestFindOwnerSelf;
    procedure TestFindSubA;
    procedure TestFindSubB;
    procedure TestFindSubNoC;
  end;

implementation
{$DEFINE USENEW}
{$IFDEF USENEW}
Function FindNestedComponent(Root : TComponent; APath : String; CStyle : Boolean = True) : TComponent;

  Function GetNextName : String; inline;
  
  Var
    P : Integer;
    CM : Boolean;
    
  begin
    P:=Pos('.',APath);
    CM:=False;
    If (P=0) then
      begin
      If CStyle then
        begin
        P:=Pos('->',APath);
        CM:=P<>0;
        end;
      If (P=0) Then
        P:=Length(APath)+1;
      end;
    Result:=Copy(APath,1,P-1);
    Delete(APath,1,P+Ord(CM));
  end;

Var
  C : TComponent;
  S : String;
begin
  If (APath='') then
    Result:=Nil
  else
    begin
    Result:=Root;
    While (APath<>'') And (Result<>Nil) do
      begin
      C:=Result;
      S:=Uppercase(GetNextName);
      Result:=C.FindComponent(S);
      If (Result=Nil) And (S='OWNER') then
        Result:=C;
      end;
    end;
end;
{$ENDIF}

procedure TTestFindComponent.TestEmpty;

begin
  // Delphi crashes on this test, don't think we should copy that :-)
  CheckFind(R,'',Nil);
end;

procedure TTestFindComponent.TestFindA;

begin
  CheckFind(R,'AAAA',A);
end;

procedure TTestFindComponent.TestFindB;

begin
  CheckFind(R,'BBBB',B);
end;

procedure TTestFindComponent.TestFindACaseDiffer;
begin
  CheckFind(R,'aaaa',A);
end;

procedure TTestFindComponent.TestFindBCaseDiffer;
begin
  CheckFind(R,'bbbb',B);
end;

procedure TTestFindComponent.TestFindNonExistSub;
begin
  CheckFind(R,'aaaa.bbbb',Nil);
end;

procedure TTestFindComponent.TestFindNonExist;
begin
  CheckFind(R,'qqqq',Nil);
end;

procedure TTestFindComponent.TestFindSubA;
begin
  CheckFind(R,'aaaa.cccc',ac);
end;

procedure TTestFindComponent.TestFindSubB;
begin
  CheckFind(R,'bbbb.cccc',bc);
end;

procedure TTestFindComponent.TestFindSubNoC;
begin
  CheckFind(R,'cccc',nil);
end;

procedure TTestFindComponent.TestFindOwnerNamed;
begin
  CheckFind(R,'BBBB.OWNER',D);
end;

procedure TTestFindComponent.TestFindOwner;
begin
  CheckFind(B,'OWNER',D);
end;

procedure TTestFindComponent.TestFindOwnerSelf;
begin
  CheckFind(A,'OWNER',A);
end;

procedure TTestFindComponent.TestFindOwnerNameOwner;
begin
  CheckFind(B,'OWNER.OWNER',D);
end;

function TTestFindComponent.CreateNamed(AOwner: TComponent; AName: String
  ): TComponent;
begin
  Result:=TComponent.Create(AOwner);
  Result.Name:=AName;
end;

procedure TTestFindComponent.CheckFind(Root: TComponent; AName: String;
  Expected: TComponent);
  
  Function FN (C : TComponent): String;
  
  begin
    If (C=Nil) then
      Result:='<Nil>'
    else
      Result:=C.GetNamePath;
  end;

Var
  Res : TComponent;
  
begin
  Res:=FindNestedComponent(Root,AName);
  If Res<>Expected then
    Fail('Search for "'+AName+'" failed : Found "'+FN(Res)+'", expected : "'+Fn(Expected)+'"');
end;

procedure TTestFindComponent.SetUp;
begin
  R:=CreateNamed(Nil,'Root');
  A:=CreateNamed(R,'AAAA');
  B:=CreateNamed(R,'BBBB');
  AC:=CreateNamed(A,'CCCC');
  BC:=CreateNamed(B,'CCCC');
  D:=CreateNamed(B,'OWNER');
  inherited SetUp;
end;

procedure TTestFindComponent.TearDown;
begin
  FreeAndNil(R); // Will free the rest.
  A:=Nil;
  B:=Nil;
  AC:=Nil;
  BC:=Nil;
  D:=Nil;
end;


initialization

  RegisterTest(TTestFindComponent); 
end.

