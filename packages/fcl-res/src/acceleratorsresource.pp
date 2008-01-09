{
    This file is part of the Free Pascal run time library.
    Copyright (c) 2008 by Giulio Bernardi

    Accelerator table resource type

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}

unit acceleratorsresource;

{$MODE OBJFPC}

interface

uses
  Classes, SysUtils, resource;

const
  FVirtKey  =  1;
  FNoInvert =  2;
  FShift    =  4;
  FControl  =  8;
  FAlt      = 16;

type
  TAccelerator = packed record
    Flags : word;
    Ansi : word;
    Id : word;
    padding : word;
  end;
  PAccelerator = ^TAccelerator;
  
type

  { TAcceleratorsResource }

  TAcceleratorsResource = class(TAbstractResource)
  private
    fType : TResourceDesc;
    fName : TResourceDesc;
    fList : TFPList;
    procedure CheckDataLoaded;
    function GetCount : integer;
    function GetItem(index : integer) : TAccelerator;
    procedure SetItem(index : integer; aAccelerator : TAccelerator);
  protected
    function GetType : TResourceDesc; override;
    function GetName : TResourceDesc; override;
    function ChangeDescTypeAllowed(aDesc : TResourceDesc) : boolean; override;
    function ChangeDescValueAllowed(aDesc : TResourceDesc) : boolean; override;
    procedure NotifyResourcesLoaded; override;
  public
    constructor Create; override;
    constructor Create(aType,aName : TResourceDesc); override;
    destructor Destroy; override;
    procedure UpdateRawData; override;
    procedure Add(aItem : TAccelerator);
    procedure Clear;
    procedure Delete(aIndex : integer);
    property Count : integer read GetCount;
    property Items[index : integer] : TAccelerator read GetItem write SetItem; default;
  end;


implementation

uses
  resfactory;

{ TAcceleratorsResource }

procedure TAcceleratorsResource.CheckDataLoaded;
var acc : TAccelerator;
    tot, i : integer;
    p : PAccelerator;
begin
  if fList<>nil then exit;
  fList:=TFPList.Create;
  if RawData.Size=0 then exit;
  RawData.Position:=0;
  tot:=RawData.Size div 8;
  for i:=1 to tot do
  begin
    RawData.ReadBuffer(acc,sizeof(acc));
    {$IFDEF ENDIAN_BIG}
    acc.Flags:=SwapEndian(acc.Flags);
    acc.Ansi:=SwapEndian(acc.Ansi);
    acc.Id:=SwapEndian(acc.Id);
    acc.padding:=SwapEndian(acc.padding);
    {$ENDIF}
    GetMem(p,sizeof(TAccelerator));
    p^:=acc;
    fList.Add(p);
  end;
end;

function TAcceleratorsResource.GetCount: integer;
begin
  CheckDataLoaded;
  Result:=fList.Count;
end;

function TAcceleratorsResource.GetItem(index: integer): TAccelerator;
begin
  CheckDataLoaded;
  Result:=PAccelerator(fList[index])^;
end;

procedure TAcceleratorsResource.SetItem(index: integer;
  aAccelerator: TAccelerator);
begin
  CheckDataLoaded;
  PAccelerator(fList[index])^:=aAccelerator;
end;

function TAcceleratorsResource.GetType: TResourceDesc;
begin
  Result:=fType;
end;

function TAcceleratorsResource.GetName: TResourceDesc;
begin
  Result:=fName;
end;

function TAcceleratorsResource.ChangeDescTypeAllowed(aDesc: TResourceDesc
  ): boolean;
begin
  Result:=aDesc=fName;
end;

function TAcceleratorsResource.ChangeDescValueAllowed(aDesc: TResourceDesc
  ): boolean;
begin
  Result:=aDesc=fName;
end;

procedure TAcceleratorsResource.NotifyResourcesLoaded;
begin
end;

constructor TAcceleratorsResource.Create;
begin
  inherited Create;
  fList:=nil;
  fType:=TResourceDesc.Create(RT_ACCELERATOR);
  fName:=TResourceDesc.Create(1);
  SetDescOwner(fType);
  SetDescOwner(fName);
end;

constructor TAcceleratorsResource.Create(aType, aName: TResourceDesc);
begin
  Create;
  fName.Assign(aName);
end;

destructor TAcceleratorsResource.Destroy;
begin
  fType.Free;
  fName.Free;
  if fList<>nil then
  begin
    Clear;
    fList.Free;
  end;
  inherited Destroy;
end;

procedure TAcceleratorsResource.UpdateRawData;
var acc : TAccelerator;
    i : integer;
begin
  if fList=nil then exit;
  RawData.Size:=0;
  RawData.Position:=0;

  if fList.Count>0 then
    for i:=0 to fList.Count-1 do
    begin
      acc:=PAccelerator(fList[i])^;
      // $80 means 'this is the last entry', so be sure only the last one has this bit set.
      if i=Count-1 then acc.Flags:=acc.Flags or $80
      else acc.Flags:=acc.Flags and $7F;
      
      {$IFDEF ENDIAN_BIG}
      acc.Flags:=SwapEndian(acc.Flags);
      acc.Ansi:=SwapEndian(acc.Ansi);
      acc.Id:=SwapEndian(acc.Id);
      acc.padding:=SwapEndian(acc.padding);
      {$ENDIF}
      RawData.WriteBuffer(acc,sizeof(acc));
    end;
  Clear;
  FreeAndNil(fList);
end;

procedure TAcceleratorsResource.Add(aItem: TAccelerator);
var p : PAccelerator;
begin
  CheckDataLoaded;
  GetMem(p,sizeof(TAccelerator));
  p^:=aItem;
  fList.Add(p);
end;

procedure TAcceleratorsResource.Clear;
var p : PAccelerator;
    i : integer;
begin
  CheckDataLoaded;
  for i:=0 to fList.Count-1 do
  begin
    p:=PAccelerator(fList[i]);
    FreeMem(p);
  end;
  fList.Clear;
end;

procedure TAcceleratorsResource.Delete(aIndex: integer);
var p : PAccelerator;
begin
  CheckDataLoaded;
  p:=PAccelerator(fList[aIndex]);
  FreeMem(p);
  fList.Delete(aIndex);
end;

initialization
  TResourceFactory.RegisterResourceClass(RT_ACCELERATOR,TAcceleratorsResource);

end.
