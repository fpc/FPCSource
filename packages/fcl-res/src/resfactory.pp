{
    This file is part of the Free Pascal run time library.
    Copyright (c) 2008 by Giulio Bernardi

    Factory class for resources

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}

unit resfactory;

{$MODE OBJFPC}

interface

uses
  Classes, SysUtils, resource;

type
  EResourceFactoryException = class(EResourceException);
  EResourceClassAlreadyRegisteredException = class(EResourceFactoryException);
  
resourcestring
  SAlreadyRegistered = 'A resource class for the type %s is already registered.';
  
type

  { TResourceFactory }

  TResourceFactory = class
  private
    class procedure InitResTypeList;
    class procedure DisposeResTypeList;
    class function FindResourceClass(aType : TResourceDesc) : TResourceClass;
  protected
  public
    class procedure RegisterResourceClass(aType : TResID; aClass : TResourceClass); overload;
    class procedure RegisterResourceClass(aType : TResName; aClass : TResourceClass); overload;
    class procedure RegisterResourceClass(aType : TResourceDesc; aClass : TResourceClass); overload;
    class function CreateResource(aType, aName : TResourceDesc) : TAbstractResource;
  end;

implementation

type
  TRegisteredResourceEntry = record
    _type : TResourceDesc;
    _class : TResourceClass;
  end;
  PRegisteredResourceEntry = ^TRegisteredResourceEntry;
  
var
  ResTypeList : TFPList = nil;


{ TResourceFactory }

class procedure TResourceFactory.InitResTypeList;
begin
  if ResTypeList=nil then
    ResTypeList:=TFPList.Create;
end;

class procedure TResourceFactory.DisposeResTypeList;
var i : integer;
    p : PRegisteredResourceEntry;
begin
  if ResTypeList=nil then exit;
  for i:=0 to ResTypeList.Count-1 do
  begin
    p:=PRegisteredResourceEntry(ResTypeList[i]);
    p^._type.Free;
    FreeMem(p);
  end;
  FreeAndNil(ResTypeList);
end;

class function TResourceFactory.FindResourceClass(aType: TResourceDesc
  ): TResourceClass;
var i : integer;
    p : PRegisteredResourceEntry;
begin
  InitResTypeList;
  for i:=0 to ResTypeList.Count-1 do
  begin
    p:=PRegisteredResourceEntry(ResTypeList[i]);
    if p^._type.Equals(aType) then
    begin
      Result:=p^._class;
      exit;
    end;
  end;
  Result:=nil;
end;

class procedure TResourceFactory.RegisterResourceClass(aType : TResID;
  aClass : TResourceClass); overload;
var t : TResourceDesc;
begin
  t:=TResourceDesc.Create(aType);
  try
    RegisterResourceClass(t,aClass);
  finally
    t.Free;
  end;
end;

class procedure TResourceFactory.RegisterResourceClass(aType : TResName;
  aClass : TResourceClass); overload;
var t : TResourceDesc;
begin
  t:=TResourceDesc.Create(aType);
  try
    RegisterResourceClass(t,aClass);
  finally
    t.Free;
  end;
end;

class procedure TResourceFactory.RegisterResourceClass(aType: TResourceDesc;
  aClass: TResourceClass);
var p : PRegisteredResourceEntry;
begin
  if FindResourceClass(aType)<>nil then
    raise EResourceClassAlreadyRegisteredException.CreateFmt(SAlreadyRegistered,[aType.Name]);
  p:=GetMem(sizeof(TRegisteredResourceEntry));
  p^._type:=TResourceDesc.Create;
  p^._type.Assign(aType);
  p^._class:=aClass;
  ResTypeList.Add(p);
end;

class function TResourceFactory.CreateResource(aType, aName: TResourceDesc
  ): TAbstractResource;
var theclass : TResourceClass;
begin
  theclass:=FindResourceClass(aType);
  if theclass=nil then theclass:=TGenericResource;
  Result:=theclass.Create(aType,aName);
end;

finalization
  TResourceFactory.DisposeResTypeList;

end.
