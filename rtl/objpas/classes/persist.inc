{
    This file is part of the Free Component Library (FCL)
    Copyright (c) 1999-2000 by the Free Pascal development team

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}

{****************************************************************************}
{*                             TPersistent                                  *}
{****************************************************************************}

procedure TPersistent.AssignError(Source: TPersistent);

Var SourceName : String;

begin
  If Source<>Nil then
    SourceName:=Source.ClassName
  else
    SourceName:='Nil';
  raise EConvertError.CreateFmt (SAssignError,[SourceName,ClassName]);
end;



procedure TPersistent.AssignTo(Dest: TPersistent);


begin
  Dest.AssignError(Self);
end;


procedure TPersistent.DefineProperties(Filer: TFiler);

begin
end;


function  TPersistent.GetOwner: TPersistent;

begin
  Result:=Nil;
end;

destructor TPersistent.Destroy;

begin
  Inherited Destroy;
end;


procedure TPersistent.Assign(Source: TPersistent);

begin
  If Source<>Nil then
    Source.AssignTo(Self)
  else
    AssignError(Nil);
end;

function  TPersistent.GetNamePath: string;

Var OwnerName :String;

begin
 Result:=ClassNAme;
 If GetOwner<>Nil then
   begin
   OwnerName:=GetOwner.GetNamePath;
   If OwnerName<>'' then Result:=OwnerName+'.'+Result;
   end;
end;


{****************************************************************************}
{*                          TInterfacedPersistent                           *}
{****************************************************************************}

procedure TInterfacedPersistent.AfterConstruction;
begin
  inherited;
  if assigned(GetOwner) then
    GetOwner.GetInterface(IUnknown,FOwnerInterface);
end;


function TInterfacedPersistent._AddRef: Integer;stdcall;
begin
  if assigned(FOwnerInterface) then
    Result:=FOwnerInterface._AddRef
  else
    Result:=-1;
end;


function TInterfacedPersistent._Release: Integer;stdcall;
begin
  if assigned(FOwnerInterface) then
    Result:=FOwnerInterface._Release
  else
    Result:=-1;
end;


function TInterfacedPersistent.QueryInterface(const IID: TGUID; out Obj): HResult;stdcall;
begin
  if GetInterface(IID, Obj) then
    Result:=0
  else
    Result:=HResult($80004002);
end;


{****************************************************************************}
{*                                TRecall                                   *}
{****************************************************************************}

constructor TRecall.Create(AStorage,AReference: TPersistent);
begin
  inherited Create;
  FStorage:=AStorage;
  FReference:=AReference;
  Store;
end;


destructor TRecall.Destroy;
begin
  if Assigned(FReference) then
   FReference.Assign(FStorage);
  Forget;
  inherited;
end;


procedure TRecall.Forget;
begin
  FReference:=nil;
  FreeAndNil(FStorage);
end;


procedure TRecall.Store;
begin
  if Assigned(FReference) then
    FStorage.Assign(FReference);
end;


