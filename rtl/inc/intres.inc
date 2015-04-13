{
    This file is part of the Free Pascal run time library.
    Copyright (c) 2008 by Giulio Bernardi

    Resource support for non-PECOFF targets (ELF, Mach-O)

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}
type
  PResInfoNode = ^TResInfoNode;
  TResInfoNode = packed record
    nameid : PChar;             //name / integer ID / languageID
    ncounthandle : longword;    //named sub-entries count / resource handle
    idcountsize : longword;     //id sub-entries count / resource size
    subptr : PResInfoNode;      //first sub-entry pointer
  end;

  TResHdr = packed record
    rootptr     : PResInfoNode; //pointer to root node
    count       : longword;     //number of resources in the file
    usedhandles : longword;     //last resource handle used
    handles     : PPtrUint;     //pointer to handles
  end;
  PResHdr = ^TResHdr;

  TLibGetResHdr=function():PResHdr;

var
{$ifdef FPC_HAS_WINLIKERESOURCES}
  ResHeader : PResHdr; external name 'FPC_RESLOCATION';
{$else}
  ResHeader : PResHdr= nil;
{$endif}

(*****************************************************************************
                             Private Helper Functions
*****************************************************************************)

function ExtGetResHdr(ModuleHandle : TFPResourceHMODULE):PResHdr;
var
  p:TLibGetResHdr;
  pp:pointer;
begin
  ExtGetResHdr:=nil;
  if ModuleHandle=0 then
    ExtGetResHdr:=ResHeader // internal
  else
  begin
    // 1-st way to get resource location
    p:=TLibGetResHdr(GetProcAddress(ModuleHandle,'rsrc'));
    if p<>nil then // there is public
      ExtGetResHdr:=p();
    if ExtGetResHdr=nil then // try another way
    begin
      // 2-nd way to get resource location
      pp:=GetProcAddress(ModuleHandle,'FPC_RESLOCATION');
      if pp<>nil then
      ExtGetResHdr:=PResHDR(pp^);
    end;
  end;
end;


//resource functions are case insensitive... copied from genstr.inc
function ResStrIComp(Str1, Str2 : PChar): SizeInt;
var
  counter: SizeInt;
  c1, c2: char;
begin
  counter := 0;
  c1 := upcase(str1[counter]);
  c2 := upcase(str2[counter]);
  while c1 = c2 do
  begin
    if (c1 = #0) or (c2 = #0) then break;
    inc(counter);
    c1 := upcase(str1[counter]);
    c2 := upcase(str2[counter]);
  end;
  ResStrIComp := ord(c1) - ord(c2);
end;

{!fixme!}
//function InternalIsIntResource(aStr : pchar; out aInt : PtrUint) : boolean;
function InternalIsIntResource(aStr : pchar; var aInt : PtrUint) : boolean;
var i : integer;
    s : shortstring;
    code : word;
begin
  InternalIsIntResource:=((PtrUInt(aStr) shr 16)=0);
  if InternalIsIntResource then aInt:=PtrUInt(aStr)
  else
  begin
    //a string like #number specifies an integer id
    if aStr[0]='#' then
    begin
      i:=1;
      while aStr[i]<>#0 do
        inc(i);
      if i>256 then i:=256;
      s[0]:=chr(i-1);
      Move(aStr[1],s[1],i-1);
      Val(s,aInt,code);
      InternalIsIntResource:=code=0;
    end;
  end;
end;

function BinSearchStr(arr : PResInfoNode; query : pchar; left, right : integer)
: PResInfoNode;
var pivot, res : integer;
    resstr : pchar;
begin
  BinSearchStr:=nil;
  while left<=right do
  begin
    pivot:=(left+right) div 2;
    resstr:=arr[pivot].nameid;
    res:=ResStrIComp(resstr,query);
    if res<0 then left:=pivot+1
    else if res>0 then right:=pivot-1
    else
    begin
      BinSearchStr:=@arr[pivot];
      exit;
    end;
  end;
end;

function BinSearchInt(arr : PResInfoNode; query : pchar; left, right : integer)
: PResInfoNode;
var pivot : integer;
begin
  BinSearchInt:=nil;
  while left<=right do
  begin
    pivot:=(left+right) div 2;
    if PtrUint(arr[pivot].nameid)<PtrUInt(query) then left:=pivot+1
    else if PtrUint(arr[pivot].nameid)>PtrUInt(query) then right:=pivot-1
    else
    begin
      BinSearchInt:=@arr[pivot];
      exit;
    end;
  end;
end;

function BinSearchRes(root : PResInfoNode; aDesc : PChar) : PResInfoNode;
var aID : PtrUint;
begin
  if InternalIsIntResource(aDesc,aID) then
    BinSearchRes:=BinSearchInt(root^.subptr,PChar(aID),root^.ncounthandle,
      root^.ncounthandle+root^.idcountsize-1)
  else
    BinSearchRes:=BinSearchStr(root^.subptr,aDesc,0,root^.ncounthandle-1);
end;

//Returns a pointer to a name node.
function InternalFindResource(ResHdr:PResHdr;ResourceName, ResourceType: PChar):
 PResInfoNode;
begin
  InternalFindResource:=nil;
  if ResHdr=nil then exit;
  InternalFindResource:=ResHdr^.rootptr;

  InternalFindResource:=BinSearchRes(InternalFindResource,ResourceType);
  if InternalFindResource<>nil then
    InternalFindResource:=BinSearchRes(InternalFindResource,ResourceName);
end;

function FindSubLanguage(aPtr : PResInfoNode; aLangID : word; aMask: word) : PResInfoNode;
var arr : PResInfoNode;
    i : longword;
begin
  FindSubLanguage:=nil;
  arr:=aPtr^.subptr;
  i:=0;
  while i<aPtr^.idcountsize do
  begin
    if (PtrUInt(arr[i].nameid) and aMask)=(aLangID and aMask) then
    begin
      FindSubLanguage:=@arr[i];
      exit;
    end;
    inc(i);
  end;
end;




(*****************************************************************************
                             Public Resource Functions
*****************************************************************************)

Function IntHINSTANCE : TFPResourceHMODULE;
begin
  IntHINSTANCE:=0;
end;

Function IntEnumResourceTypes(ModuleHandle : TFPResourceHMODULE; EnumFunc : EnumResTypeProc; lParam : PtrInt) : LongBool;
var ptr : PResInfoNode;
    tot, i : integer;
    res_hdr:PResHdr;
begin
  IntEnumResourceTypes:=False;
  res_hdr:=ExtGetResHdr(ModuleHandle);
  if res_hdr=nil then exit;
  tot:=res_hdr^.rootptr^.ncounthandle+res_hdr^.rootptr^.idcountsize;
  ptr:=res_hdr^.rootptr^.subptr;
  IntEnumResourceTypes:=true;
  i:=0;
  while i<tot do
  begin
    if not EnumFunc(ModuleHandle,ptr[i].nameid,lParam) then exit;
    inc(i);
  end;
end;

Function IntEnumResourceNames(ModuleHandle : TFPResourceHMODULE; ResourceType : PChar; EnumFunc : EnumResNameProc; lParam : PtrInt) : LongBool;
var ptr : PResInfoNode;
    tot, i : integer;
    res_hdr:PResHdr;
begin
  IntEnumResourceNames:=False;
  res_hdr:=ExtGetResHdr(ModuleHandle);
  if res_hdr=nil then exit;
  ptr:=res_hdr^.rootptr;

  ptr:=BinSearchRes(ptr,ResourceType);
  if ptr=nil then exit;
  
  tot:=ptr^.ncounthandle+ptr^.idcountsize;
  ptr:=ptr^.subptr;
  IntEnumResourceNames:=true;
  i:=0;
  while i<tot do
  begin
    if not EnumFunc(ModuleHandle,ResourceType,ptr[i].nameid,lParam) then exit;
    inc(i);
  end;
end;

Function IntEnumResourceLanguages(ModuleHandle : TFPResourceHMODULE; ResourceType, ResourceName : PChar; EnumFunc : EnumResLangProc; lParam : PtrInt) : LongBool;
var ptr : PResInfoNode;
    tot, i : integer;
    res_hdr:PResHdr;
begin
  IntEnumResourceLanguages:=False;
  res_hdr:=ExtGetResHdr(ModuleHandle);
  if res_hdr=nil then exit;
  ptr:=InternalFindResource(res_hdr,ResourceName,ResourceType);
  if ptr=nil then exit;

  tot:=ptr^.idcountsize;
  ptr:=ptr^.subptr;
  IntEnumResourceLanguages:=true;
  i:=0;
  while i<tot do
  begin
    if not EnumFunc(ModuleHandle,ResourceType,ResourceName,PtrUInt(ptr[i].nameid),lParam) then exit;
    inc(i);
  end;
end;

Function IntFindResource(ModuleHandle: TFPResourceHMODULE; ResourceName,
  ResourceType: PChar): TFPResourceHandle;
var ptr : PResInfoNode;
    res_hdr: PresHdr;
begin
  IntFindResource:=0;
  res_hdr:=ExtGetResHdr(ModuleHandle);
  if res_hdr=nil then exit;
  ptr:=InternalFindResource(res_hdr,ResourceName,ResourceType);
  if ptr=nil then exit;

  //first language id
  ptr:=ptr^.subptr;
  if ptr^.ncounthandle=0 then
  begin
    res_hdr^.handles[res_hdr^.usedhandles]:=PtrUint(ptr);
    inc(res_hdr^.usedhandles);
    ptr^.ncounthandle:=res_hdr^.usedhandles;
  end;
  IntFindResource:=ptr^.ncounthandle;
end;

Function IntFindResourceEx(ModuleHandle: TFPResourceHMODULE; ResourceType,
  ResourceName: PChar; Language : word): TFPResourceHandle;
const LANG_NEUTRAL = 0;
      LANG_ENGLISH = 9;
var nameptr,ptr : PResInfoNode;
    res_hdr: PResHdr;
begin
  IntFindResourceEx:=0;
  res_hdr:=ExtGetResHdr(ModuleHandle);
  if res_hdr=nil then exit;
  nameptr:=InternalFindResource(res_hdr,ResourceName,ResourceType);
  if nameptr=nil then exit;

  //try exact match
  ptr:=FindSubLanguage(nameptr,Language,$FFFF);
  //try primary language
  if ptr=nil then
    ptr:=FindSubLanguage(nameptr,Language,$3FF);
  //try language neutral
  if ptr=nil then
    ptr:=FindSubLanguage(nameptr,LANG_NEUTRAL,$3FF);
  //try english
  if ptr=nil then
    ptr:=FindSubLanguage(nameptr,LANG_ENGLISH,$3FF);
  //nothing found, return the first one
  if ptr=nil then
    ptr:=nameptr^.subptr;

  if ptr^.ncounthandle=0 then
  begin
    res_hdr^.handles[res_hdr^.usedhandles]:=PtrUint(ptr);
    inc(res_hdr^.usedhandles);
    ptr^.ncounthandle:=res_hdr^.usedhandles;
  end;
  IntFindResourceEx:=ptr^.ncounthandle;
end;

Function IntLoadResource(ModuleHandle: TFPResourceHMODULE; ResHandle: TFPResourceHandle): TFPResourceHGLOBAL;
var res_hdr: PResHdr;
begin
  IntLoadResource:=0;
  res_hdr:=ExtGetResHdr(ModuleHandle);
  if res_hdr=nil then exit;
  if (ResHandle<=0) or (ResHandle>res_hdr^.usedhandles) then exit;
  IntLoadResource:=TFPResourceHGLOBAL(PResInfoNode(res_hdr^.handles[ResHandle-1])^.subptr);
end;

Function IntSizeofResource(ModuleHandle: TFPResourceHMODULE; ResHandle: TFPResourceHandle): LongWord;
var res_hdr: PResHdr;
begin
  IntSizeofResource:=0;
  res_hdr:=ExtGetResHdr(ModuleHandle);
  if res_hdr=nil then exit;
  if (ResHandle<=0) or (ResHandle>res_hdr^.usedhandles) then exit;
  IntSizeofResource:=PResInfoNode(res_hdr^.handles[ResHandle-1])^.idcountsize;
end;

Function IntLockResource(ResData: TFPResourceHGLOBAL): Pointer;
begin
  IntLockResource:=Nil;
  if ResHeader=nil then exit;
  IntLockResource:=Pointer(ResData);
end;

Function IntUnlockResource(ResData: TFPResourceHGLOBAL): LongBool;
begin
  IntUnlockResource:=(ResHeader<>nil);
end;

Function IntFreeResource(ResData: TFPResourceHGLOBAL): LongBool;
begin
  IntFreeResource:=(ResHeader<>nil);
end;

const
  InternalResourceManager : TResourceManager =
  (
    HINSTANCEFunc : @IntHINSTANCE;
    EnumResourceTypesFunc : @IntEnumResourceTypes;
    EnumResourceNamesFunc : @IntEnumResourceNames;
    EnumResourceLanguagesFunc : @IntEnumResourceLanguages;
    FindResourceFunc : @IntFindResource;
    FindResourceExFunc : @IntFindResourceEx;
    LoadResourceFunc : @IntLoadResource;
    SizeofResourceFunc : @IntSizeofResource;
    LockResourceFunc : @IntLockResource;
    UnlockResourceFunc : @IntUnlockResource;
    FreeResourceFunc : @IntFreeResource;
  );
