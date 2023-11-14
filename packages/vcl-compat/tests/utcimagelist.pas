unit utcImagelist;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, fpcunit, testutils, testregistry, system.imagelist;

type
  
  { TMyImageList }

  TMyImageList = class(TBaseImageList)
  private
    FDidChange: Boolean;
  protected
    procedure DoChange; override;
    function GetCount: Integer; override;
    Property DidChange : Boolean Read FDidChange;
    property LinkCount;
    property Links;
  end;

  { TestBaseImageList }

  TestBaseImageList= class(TTestCase)
  private
    FLink1: TImageLink;
    FLink2: TImageLink;
    FList1: TBaseImageList;
    FList2: TBaseImageList;
    FLink1Change : TObject;
    procedure FreeLink1;
    procedure FreeLink2;
    procedure Link1Changed(Sender: TObject);
  protected
    procedure SetUp; override;
    procedure TearDown; override;
    Property List1 : TBaseImageList Read FList1;
    Property List2 : TBaseImageList Read FList2;
    Property Link1 : TImageLink Read FLink1;
    Property Link2 : TImageLink Read FLink2;
  published
    procedure TestHookUp;
    procedure TestSetLink;
    procedure TestChangeLink;
    procedure TestSetLinkIgnoreChange;
    procedure TestFreeList;
    procedure TestFreeLink;
    procedure TestSetImageIndex;
  end;

implementation

{ TMyImageList }

procedure TMyImageList.DoChange;
begin
  FDidchange:=True;
end;

function TMyImageList.GetCount: Integer;
begin
  Result:=0;
end;

procedure TestBaseImageList.TestHookUp;

begin
  AssertNull('No change in link 1',FLink1Change);
  AssertNotNull('Link 1',Link1);
  AssertNotNull('Link 2',Link2);
  AssertNotNull('List 1',List1);
  AssertNotNull('List 2',List1);
end;

procedure TestBaseImageList.TestSetLink;
begin
  Link1.Images:=List1;
  AssertSame('Assigned link 1', List1,Link1.Images);
  AssertEquals('Count list 1', 1, TMyImageList(List1).LinkCount);
  AssertFalse('changed list 1', TMyImageList(List1).DidChange);
  AssertSame('Link 1 changed',List1,FLink1Change);
  Link2.Images:=List2;
  AssertSame('Assigned link 2', List2,Link2.Images);
  AssertEquals('Count list 2', 1, TMyImageList(List2).LinkCount);
  AssertFalse('changed list 2', TMyImageList(List2).DidChange);
end;

procedure TestBaseImageList.TestChangeLink;
begin
  Link1.Images:=List1;
  AssertSame('Assigned link 1', List1,Link1.Images);
  AssertEquals('Count list 1', 1, TMyImageList(List1).LinkCount);
  AssertFalse('changed list 1', TMyImageList(List1).DidChange);
  Link2.Images:=List2;
  AssertSame('Assigned link 2', List2,Link2.Images);
  AssertEquals('Count list 2', 1, TMyImageList(List2).LinkCount);
  AssertFalse('changed list 2', TMyImageList(List2).DidChange);
  Link2.Images:=List1;
  AssertSame('Assigned link 1', List1,Link1.Images);
  AssertEquals('Count list 1', 2, TMyImageList(List1).LinkCount);
  AssertFalse('changed list 1', TMyImageList(List1).DidChange);
  AssertEquals('Count list 2', 0, TMyImageList(List2).LinkCount);
  AssertFalse('changed list 2', TMyImageList(List2).DidChange);
end;

procedure TestBaseImageList.TestSetLinkIgnoreChange;
begin
  Link1.IgnoreImages:=True;
  Link1.Images:=List1;
  AssertSame('Assigned link 1', List1,Link1.Images);
  AssertEquals('Count list 1', 1, TMyImageList(List1).LinkCount);
  AssertFalse('changed list 1', TMyImageList(List1).DidChange);
  AssertNull('Link 1 not changed',FLink1Change);
end;

procedure TestBaseImageList.TestFreeList;
begin
  Link1.Images:=List1;
  Link2.Images:=List1;
  FreeAndNil(Flist1);
  AssertNull('Link 1 no images',Link1.Images);
  AssertNull('Link 2 no images',Link2.Images);
end;

procedure TestBaseImageList.TestFreeLink;
begin
  Link1.Images:=List1;
  Link2.Images:=List1;
  FreeLink1;
  AssertEquals('Link 1 image count',1,TMyImageList(List1).LinkCount);
end;

procedure TestBaseImageList.TestSetImageIndex;
begin
  Link1.Images:=List1;
  Link1.ImageIndex:=1;
  AssertSame('Changed',List1,FLink1Change);
  FLink1Change:=Nil;
  Link1.ImageIndex:=1;
  AssertNull('Not Changed',FLink1Change);
  Link1.IgnoreIndex:=True;
  Link1.ImageIndex:=2;
  AssertNull('Not Changed',FLink1Change);
end;

procedure TestBaseImageList.SetUp;
begin
  FList1:=TMyImageList.Create(Nil);
  FList2:=TMyImageList.Create(Nil);
  FLink1:=TImageLink.Create;
  FLink1.OnChange:=@Link1Changed;
  FLink2:=TImageLink.Create;
  FLink1Change:=Nil;
end;

procedure TestBaseImageList.FreeLink1;

begin
  FreeAndNil(FLink1);
end;

procedure TestBaseImageList.FreeLink2;

begin
  FreeAndNil(FLink2);
end;

procedure TestBaseImageList.Link1Changed(Sender: TObject);
begin
  FLink1Change:=Sender;
end;

procedure TestBaseImageList.TearDown;
begin
  FreeLink1;
  FreeLink2;
  FreeAndNil(FList1);
  FreeAndNil(FList2);
  inherited TearDown;
end;


initialization

  RegisterTest(TestBaseImageList);
end.

