{
    This file is part of the Free Component Library (FCL)
    Copyright (c) 1999-2000 by the Free Pascal development team

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}



{ Class registration routines }

procedure RegisterClass(AClass: TPersistentClass);
var
aClassname : String;
begin
  //Classlist is created during initialization.
  with Classlist.Locklist do
     try
      while Indexof(AClass) = -1 do
         begin
           aClassname := AClass.ClassName;
           if GetClass(aClassName) <> nil then  //class alread registered!
                 Begin
                 //raise an error
                 exit;
                 end;
          Add(AClass);
          if AClass = TPersistent then break;
          AClass := TPersistentClass(AClass.ClassParent);
         end;
     finally
       ClassList.UnlockList;
     end;
end;


procedure RegisterClassAlias(AClass: TPersistentClass; const Alias: string);
  var
    I : integer;
  begin
    i := ClassAliasList.IndexOf(Alias);
    if I = -1 then
      ClassAliasList.AddObject( Alias, TObject(AClass) );
  end;


procedure RegisterClasses(AClasses: array of TPersistentClass);
var
I : Integer;
begin
for I := low(aClasses) to high(aClasses) do
       RegisterClass(aClasses[I]);
end;


procedure UnRegisterClass(AClass: TPersistentClass);

begin
end;


procedure UnRegisterClasses(AClasses: array of TPersistentClass);

begin
end;


procedure UnRegisterModuleClasses(Module: HMODULE);
begin
end;


function FindClass(const AClassName: string): TPersistentClass;

begin
  Result := GetClass(AClassName);
  if not Assigned(Result) then
    raise EClassNotFound.CreateFmt(SClassNotFound, [AClassName]);
end;


function GetClass(const AClassName: string): TPersistentClass;
var
I : Integer;
begin
  with ClassList.LockList do
   try
    for I := 0 to Count-1 do
       begin
        Result := TPersistentClass(Items[I]);
        if Result.ClassNameIs(AClassName) then Exit;
       end;
       I := ClassAliasList.Indexof(AClassName);
       if I >= 0 then  //found
          Begin
          Result := TPersistentClass(ClassAliasList.Objects[i]);
          exit;
          end;
       Result := nil;
    finally
      ClassList.Unlocklist;
    end;
end;


procedure StartClassGroup(AClass: TPersistentClass);
begin
end;


procedure GroupDescendentsWith(AClass, AClassGroup: TPersistentClass);
begin
end;


function ActivateClassGroup(AClass: TPersistentClass): TPersistentClass;
begin
  Result:=nil;
end;


function ClassGroupOf(AClass: TPersistentClass): TPersistentClass;
begin
  Result:=nil;
end;


function ClassGroupOf(Instance: TPersistent): TPersistentClass;
begin
  Result:=nil;
end;


{ Component registration routines }

type
  TComponentPage = class(TCollectionItem)
  public
    Name: String;
    Classes: TList;
    destructor Destroy; override;
  end;

{ TComponentPage }

destructor TComponentPage.Destroy;
begin
  Classes.Free;
  inherited Destroy;
end;

var
  ComponentPages: TCollection;

procedure InitComponentPages;
begin
  ComponentPages := TCollection.Create(TComponentPage);
  { Add a empty page which will be used for storing the NoIcon components }
  ComponentPages.Add;
end;

procedure RegisterComponents(const Page: string;
  ComponentClasses: array of TComponentClass);
var
  i: Integer;
  pg: TComponentPage;
begin
  if Page = '' then exit;  { prevent caller from doing nonsense }

  pg := nil;
  if not Assigned(ComponentPages) then
    InitComponentPages
  else
    for i := 0 to ComponentPages.Count - 1 do
      if TComponentPage(ComponentPages.Items[i]).Name = Page then begin
        pg := TComponentPage(ComponentPages.Items[i]);
        break;
      end;

  if pg = nil then begin
    pg := TComponentPage(ComponentPages.Add);
    pg.Name := Page;
  end;

  if pg.Classes = nil then
    pg.Classes := TList.Create;

  for i := Low(ComponentClasses) to High(ComponentClasses) do
    pg.Classes.Add(ComponentClasses[i]);

  if Assigned(RegisterComponentsProc) then
    RegisterComponentsProc(Page, ComponentClasses);
end;


procedure RegisterNoIcon(ComponentClasses: array of TComponentClass);
var
  pg: TComponentPage;
  i: Integer;
begin
  if not Assigned(ComponentPages) then
    InitComponentPages;

  pg := TComponentPage(ComponentPages.Items[0]);
  if pg.Classes = nil then
    pg.Classes := TList.Create;

  for i := Low(ComponentClasses) to High(ComponentClasses) do
    pg.Classes.Add(ComponentClasses[i]);

  if Assigned(RegisterNoIconProc) then
    RegisterNoIconProc(ComponentClasses);
end;


procedure RegisterNonActiveX(ComponentClasses: array of TComponentClass;
  AxRegType: TActiveXRegType);

begin
end;


