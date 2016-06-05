{
    This file is part of the Free Pascal run time library.
    Copyright (c) 2008 by Giulio Bernardi

    Resource support

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}

(*****************************************************************************
                             Utility functions
*****************************************************************************)

function Is_IntResource(aStr : pchar) : boolean; {$ifdef SYSTEMINLINE}inline;{$endif}
begin
  Result:=((PtrUInt(aStr) shr 16)=0);
end;

function MakeLangID(primary,sub : word) : word; {$ifdef SYSTEMINLINE}inline;{$endif}
begin
  Result:=(primary and $3FF) or (sub shl 10);
end;

(*****************************************************************************
                             Overloaded functions
*****************************************************************************)

{$ifdef FPC_HAS_FEATURE_ANSISTRINGS}
Function FindResource(ModuleHandle: TFPResourceHMODULE; const ResourceName, ResourceType: AnsiString): TFPResourceHandle;

begin
  Result:=FindResource(ModuleHandle,PChar(ResourceName),PChar(ResourceType));
end;

Function FindResourceEx(ModuleHandle: TFPResourceHMODULE; const ResourceType, ResourceName: AnsiString; Language : word): TFPResourceHandle;

begin
  Result:=FindResourceEx(ModuleHandle,PChar(ResourceType),PChar(ResourceName),Language);
end;
{$endif}

(*****************************************************************************
                             Default resource support
*****************************************************************************)
{ These functions are the default ones on systems where resources aren't
  supported }

Function DefaultHINSTANCE : TFPResourceHMODULE;
begin
  Result:=0;
end;

Function DefaultEnumResourceTypes(ModuleHandle : TFPResourceHMODULE; EnumFunc : EnumResTypeProc; lParam : PtrInt) : LongBool;
begin
  Result:=False;
end;

Function DefaultEnumResourceNames(ModuleHandle : TFPResourceHMODULE; ResourceType : PChar; EnumFunc : EnumResNameProc; lParam : PtrInt) : LongBool;
begin
  Result:=False;
end;

Function DefaultEnumResourceLanguages(ModuleHandle : TFPResourceHMODULE; ResourceType, ResourceName : PChar; EnumFunc : EnumResLangProc; lParam : PtrInt) : LongBool;
begin
  Result:=False;
end;

Function DefaultFindResource(ModuleHandle: TFPResourceHMODULE; ResourceName, ResourceType: PChar): TFPResourceHandle;
begin
  Result:=0;
end;

Function DefaultFindResourceEx(ModuleHandle: TFPResourceHMODULE; ResourceType, ResourceName: PChar; Language : word): TFPResourceHandle;
begin
  Result:=0;
end;

Function DefaultLoadResource(ModuleHandle: TFPResourceHMODULE; ResHandle: TFPResourceHandle): TFPResourceHGLOBAL;
begin
  Result:=0;
end;

Function DefaultSizeofResource(ModuleHandle: TFPResourceHMODULE; ResHandle: TFPResourceHandle): LongWord;
begin
  Result:=0;
end;

Function DefaultLockResource(ResData: TFPResourceHGLOBAL): Pointer;
begin
  Result:=Nil;
end;

Function DefaultUnlockResource(ResData: TFPResourceHGLOBAL): LongBool;
begin
  Result:=False;
end;

Function DefaultFreeResource(ResData: TFPResourceHGLOBAL): LongBool;
begin
  Result:=False;
end;

(*****************************************************************************
            Resource manager and accessor functions
*****************************************************************************)

var
  resourcemanager : TResourceManager =
  (
    HINSTANCEFunc : @DefaultHINSTANCE;
    EnumResourceTypesFunc : @DefaultEnumResourceTypes;
    EnumResourceNamesFunc : @DefaultEnumResourceNames;
    EnumResourceLanguagesFunc : @DefaultEnumResourceLanguages;
    FindResourceFunc : @DefaultFindResource;
    FindResourceExFunc : @DefaultFindResourceEx;
    LoadResourceFunc : @DefaultLoadResource;
    SizeofResourceFunc : @DefaultSizeofResource;
    LockResourceFunc : @DefaultLockResource;
    UnlockResourceFunc : @DefaultUnlockResource;
    FreeResourceFunc : @DefaultFreeResource;
  );

procedure GetResourceManager (Var Manager : TResourceManager);
begin
  Manager:=resourcemanager;
end;

procedure SetResourceManager (Const New : TResourceManager);
begin
  resourcemanager:=New;
end;

(*****************************************************************************
                             Resources function
*****************************************************************************)
{ These functions will call the specified resource manager functions }

Function HINSTANCE : TFPResourceHMODULE;
begin
  Result:=resourcemanager.HINSTANCEFunc();
end;

Function EnumResourceTypes(ModuleHandle : TFPResourceHMODULE; EnumFunc : EnumResTypeProc; lParam : PtrInt) : LongBool;
begin
  Result:=resourcemanager.EnumResourceTypesFunc(ModuleHandle,EnumFunc,lParam);
end;

Function EnumResourceNames(ModuleHandle : TFPResourceHMODULE; ResourceType : PChar; EnumFunc : EnumResNameProc; lParam : PtrInt) : LongBool;
begin
  Result:=resourcemanager.EnumResourceNamesFunc(ModuleHandle,ResourceType,EnumFunc,lParam);
end;

Function EnumResourceLanguages(ModuleHandle : TFPResourceHMODULE; ResourceType, ResourceName : PChar; EnumFunc : EnumResLangProc; lParam : PtrInt) : LongBool;
begin
  Result:=resourcemanager.EnumResourceLanguagesFunc(ModuleHandle,ResourceType,ResourceName,EnumFunc,lParam);
end;

Function FindResource(ModuleHandle: TFPResourceHMODULE; ResourceName, ResourceType: PChar): TFPResourceHandle;
begin
  Result:=resourcemanager.FindResourceFunc(ModuleHandle,ResourceName,ResourceType);
end;

Function FindResourceEx(ModuleHandle: TFPResourceHMODULE; ResourceType, ResourceName: PChar; Language : word): TFPResourceHandle;
begin
  Result:=resourcemanager.FindResourceExFunc(ModuleHandle,ResourceType,ResourceName,Language);
end;

Function LoadResource(ModuleHandle: TFPResourceHMODULE; ResHandle: TFPResourceHandle): TFPResourceHGLOBAL;
begin
  Result:=resourcemanager.LoadResourceFunc(ModuleHandle,ResHandle);
end;

Function SizeofResource(ModuleHandle: TFPResourceHMODULE; ResHandle: TFPResourceHandle): LongWord;
begin
  Result:=resourcemanager.SizeofResourceFunc(ModuleHandle,ResHandle);
end;

Function LockResource(ResData: TFPResourceHGLOBAL): Pointer;
begin
  Result:=resourcemanager.LockResourceFunc(ResData);
end;

Function UnlockResource(ResData: TFPResourceHGLOBAL): LongBool;
begin
  Result:=resourcemanager.UnlockResourceFunc(ResData);
end;

Function FreeResource(ResData: TFPResourceHGLOBAL): LongBool;
begin
  Result:=resourcemanager.FreeResourceFunc(ResData);
end;

