{
    This file is part of the Free Pascal run time library.
    Copyright (c) 2008 by Giulio Bernardi

    Resource support as external files, for Mac OS X

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}
{
  This file is similar to extres.inc when EXTRES_MMAP is defined.
  However, two files are searched (an architecture-dependent one and a shared
  one). They are searched first in Contents/Resources directory of the program
  application bundle and then in the same directory of the program.
}

const
  FPCRES_MAGIC = 'FPCRES';
  FPCRES_VERSION = 1;
  {$IFDEF ENDIAN_BIG}
  FPCRES_ENDIAN = 1;
  {$ENDIF}
  {$IFDEF ENDIAN_LITTLE}
  FPCRES_ENDIAN = 2;
  {$ENDIF}
  FPCRES_EXT = '.fpcres';
  FPCRES_ARCH =
    {$if defined(cpui386)}
    '.i386';
    {$elseif defined(cpux86_64)}
    '.x86_64';
    {$elseif defined(cpupowerpc32)}
    '.powerpc';
    {$elseif defined(cpupowerpc64)}
    '.powerpc64';
    {$elseif defined(cpuarm)}
    '.arm';
    {$elseif defined(cpuaarch64)}
    '.aarch64';
    {$else}
    {$error add support for cpu architecture}
    {$endif}

type
  TExtHeader = packed record
    magic : array[0..5] of char;//'FPCRES'
    version : byte;             //EXT_CURRENT_VERSION
    endianess : byte;           //EXT_ENDIAN_BIG or EXT_ENDIAN_LITTLE
    count : longword;           //resource count
    nodesize : longword;        //size of header (up to string table, excluded)
    hdrsize  : longword;        //size of header (up to string table, included)
    reserved1 : longword;
    reserved2 : longword;
    reserved3 : longword;
  end;
  PExtHeader = ^TExtHeader;

  TResInfoNode = packed record
    nameid : longword;          //name offset / integer ID / languageID
    ncounthandle : longword;    //named sub-entries count/resource handle
    idcountsize : longword;     //id sub-entries count / resource size
    subptr : longword;          //first sub-entry offset
  end;
  PResInfoNode = ^TResInfoNode;

  TResFileInfo = record
    ResHeader : PExtHeader;
    fd : integer;
    size : longword;
  end;

var
  ResFileInfo     : TResFileInfo = (ResHeader : nil; fd : 0; size : 0);
  ResFileInfoArch : TResFileInfo = (ResHeader : nil; fd : 0; size : 0);
  reshandles : PPointer = nil;
  usedhandles : longword = 0;
  rescount : longword = 0;

(*****************************************************************************
                             Private Helper Functions
*****************************************************************************)

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

function GetResInfoPtr(base : PExtHeader; const offset : longword) : PResInfoNode; inline;
begin
  GetResInfoPtr:=PResInfoNode(PtrUInt(base)+offset);
end;

function GetPchar(base : PExtHeader; const offset : longword) : Pchar; inline;
begin
  GetPchar:=Pchar(PtrUInt(base)+offset);
end;

function GetPtr(base : PExtHeader; const offset : longword) : Pointer; inline;
begin
  GetPtr:=Pointer(PtrUInt(base)+offset);
end;

procedure FixResEndian(ResHeader : PExtHeader);
var ptr : plongword;
    blockend : plongword;
begin
  //all info nodes reside in a contiguos block of memory.
  //they are all 16 bytes long and made by longwords
  //so, simply swap each longword in the block
  ptr:=GetPtr(ResHeader,sizeof(TExtHeader));
  blockend:=GetPtr(ResHeader,ResHeader^.nodesize);
  while ptr<blockend do
  begin
    ptr^:=SwapEndian(ptr^);
    inc(ptr);
  end;
end;

function GetExtResBasePath : shortstring;
var exename : shortstring;
    len, i, extpos, namepos: integer;
begin
  GetExtResBasePath:=paramstr(0);
  len:=byte(GetExtResBasePath[0]);
  i:=len;
//  writeln('exe name is ',GetExtResBasePath);
  //find position of extension
  while (i>0) and (not (GetExtResBasePath[i] in ['.',DirectorySeparator])) do
    dec(i);
  //find position of last directory separator
  if (i>0) and (GetExtResBasePath[i]='.') then extpos:=i-1
  else extpos:=len;
  while (i>0) and (GetExtResBasePath[i] <> DirectorySeparator) do
    dec(i);
  namepos:=i;
  exename:=copy(GetExtResBasePath,i+1,extpos-i);
  dec(i);
  //is executable in 'MacOS' directory? find previous dir separator...
  while (i>0) and (GetExtResBasePath[i] <> DirectorySeparator) do
    dec(i);
  if i<0 then i:=0;
  //yes, search file in <bundle>/Contents/Resources directory
  if (namepos>i) and (copy(GetExtResBasePath,i+1,namepos-i-1)='MacOS') then
  begin
    GetExtResBasePath[0]:=Chr(i);
    GetExtResBasePath:=GetExtResBasePath+'Resources'+DirectorySeparator+exename;
  end
  else //no, search file in exe directory
    GetExtResBasePath[0]:=Chr(extpos);
//  writeln('base path is ',GetExtResBasePath);
end;

function GetExtResPathArch(const base : shortstring) : pchar;
var len : integer;
begin
  len:=byte(base[0]);
  GetExtResPathArch:=GetMem(len+length(FPCRES_ARCH)+length(FPCRES_EXT)+1);
  Move(base[1],GetExtResPathArch[0],len);
  Move(FPCRES_ARCH[1],GetExtResPathArch[len],length(FPCRES_ARCH));
  inc(len,length(FPCRES_ARCH));
  Move(FPCRES_EXT[1],GetExtResPathArch[len],length(FPCRES_EXT));
  inc(len,length(FPCRES_EXT));
  GetExtResPathArch[len]:=#0;
//  writeln('Arch-dependent resource file is ',GetExtResPathArch);
end;

function GetExtResPath(const base : shortstring) : pchar;
var len : integer;
begin
  len:=byte(base[0]);
  GetExtResPath:=GetMem(len+length(FPCRES_EXT)+1);
  Move(base[1],GetExtResPath[0],len);
  Move(FPCRES_EXT[1],GetExtResPath[len],length(FPCRES_EXT));
  inc(len,length(FPCRES_EXT));
  GetExtResPath[len]:=#0;
//  writeln('Shared resource file is ',GetExtResPath);
end;

procedure MapResFile(var aInfo : TResFileInfo; aName : pchar);
const
  PROT_READ  = 1;
  PROT_WRITE = 2;
var fdstat : stat;
begin
  aInfo.fd:=FpOpen(aName,O_RDONLY,0);
  FreeMem(aName);
//  writeln('fpopen returned ',aInfo.fd);
  if (aInfo.fd=-1) then exit;
  if FpFStat(aInfo.fd,fdstat)<>0 then
  begin
//    writeln('fpfstat failed');
    FpClose(aInfo.fd);
    exit;
  end;
//  writeln('fpfstat suceeded');
  aInfo.size:=fdstat.st_size;
  aInfo.ResHeader:=PExtHeader(Fpmmap(nil,aInfo.size,PROT_READ or PROT_WRITE,
    MAP_PRIVATE,aInfo.fd,0));
//  writeln('fpmmap returned ',PtrInt(aInfo.ResHeader));
  if PtrInt(aInfo.ResHeader)=-1 then
  begin
    FpClose(aInfo.fd);
    exit;
  end;
  if (aInfo.ResHeader^.magic<>FPCRES_MAGIC) or
    (aInfo.ResHeader^.version<>FPCRES_VERSION) then
  begin
    FpClose(aInfo.fd);
    exit;
  end;
//  writeln('magic ok');
  if aInfo.ResHeader^.endianess<>FPCRES_ENDIAN then
  begin
    aInfo.ResHeader^.count:=SwapEndian(aInfo.ResHeader^.count);
    aInfo.ResHeader^.nodesize:=SwapEndian(aInfo.ResHeader^.nodesize);
    aInfo.ResHeader^.hdrsize:=SwapEndian(aInfo.ResHeader^.hdrsize);
    FixResEndian(aInfo.ResHeader);
  end;
  inc(rescount,aInfo.ResHeader^.count);
end;

procedure InitResources;
var respathArch : pchar;
    respath : pchar;
    basepath : shortstring;
begin
  basepath:=GetExtResBasePath;
  respathArch:=GetExtResPathArch(basepath);
  respath:=GetExtResPath(basepath);
  MapResFile(ResFileInfoArch,respathArch);
  MapResFile(ResFileInfo,respath);

  if rescount=0 then exit;
  reshandles:=GetMem(sizeof(Pointer)*rescount);
  FillByte(reshandles^,sizeof(Pointer)*rescount,0);
end;

procedure FinalizeResources;
begin
  if (ResFileInfoArch.Resheader=nil) and (ResFileInfo.Resheader=nil) then exit;
  FreeMem(reshandles);
  if ResFileInfoArch.Resheader<>nil then
  begin
    Fpmunmap(ResFileInfoArch.ResHeader,ResFileInfoArch.size);
    FpClose(ResFileInfoArch.fd);
  end;
  if ResFileInfo.Resheader<>nil then
  begin
    Fpmunmap(ResFileInfo.ResHeader,ResFileInfo.size);
    FpClose(ResFileInfo.fd);
  end;
end;

function BinSearchStr(base : PExtHeader; arr : PResInfoNode; query : pchar;
  left, right : integer) : PResInfoNode;
var pivot, res : integer;
    resstr : pchar;
begin
  BinSearchStr:=nil;
  while left<=right do
  begin
    pivot:=(left+right) div 2;
    resstr:=GetPchar(base,arr[pivot].nameid);
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
    if arr[pivot].nameid<PtrUInt(query) then left:=pivot+1
    else if arr[pivot].nameid>PtrUInt(query) then right:=pivot-1
    else
    begin
      BinSearchInt:=@arr[pivot];
      exit;
    end;
  end;
end;

function BinSearchRes(base : PExtHeader; root : PResInfoNode; aDesc : PChar)
: PResInfoNode;
var aID : PtrUint;
begin
  if InternalIsIntResource(aDesc,aID) then
    BinSearchRes:=BinSearchInt(GetResInfoPtr(base,root^.subptr),PChar(aID),
      root^.ncounthandle,root^.ncounthandle+root^.idcountsize-1)
  else
    BinSearchRes:=BinSearchStr(base,GetResInfoPtr(base,root^.subptr),aDesc,0,
      root^.ncounthandle-1);
end;

function FindSubLanguage(base : PExtHeader; aPtr : PResInfoNode; aLangID : word;
  aMask: word) : PResInfoNode;
var arr : PResInfoNode;
    i : longword;
begin
  FindSubLanguage:=nil;
  arr:=GetResInfoPtr(base,aPtr^.subptr);
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

//Returns a pointer to a name node.
function InternalFindResource(base : PExtHeader; ResourceName, ResourceType: PChar):
 PResInfoNode;
begin
  InternalFindResource:=nil;
  if base=nil then exit;
  InternalFindResource:=GetResInfoPtr(base,sizeof(TExtHeader));

  InternalFindResource:=BinSearchRes(base,InternalFindResource,ResourceType);
  if InternalFindResource<>nil then
    InternalFindResource:=BinSearchRes(base,InternalFindResource,ResourceName);
end;

function FindResourceSingleFile(ResHeader : PExtHeader; ResourceName,
  ResourceType: PChar) : TFPResourceHandle;
var ptr : PResInfoNode;
begin
  FindResourceSingleFile:=0;
  ptr:=InternalFindResource(ResHeader,ResourceName,ResourceType);
  if ptr=nil then exit;

  //first language id
  ptr:=GetResInfoPtr(ResHeader,ptr^.subptr);
  if ptr^.ncounthandle=0 then
  begin
    reshandles[usedhandles]:=ptr;
    inc(usedhandles);
    ptr^.ncounthandle:=usedhandles;
  end;
  FindResourceSingleFile:=ptr^.ncounthandle;
end;

{!fixme!}
//function FindResourceExSingleFile(ResHeader : PExtHeader; ResourceType,
//  ResourceName: PChar;  Language : word; out precision : integer): TFPResourceHandle;
function FindResourceExSingleFile(ResHeader : PExtHeader; ResourceType,
  ResourceName: PChar;  Language : word; var precision : integer): TFPResourceHandle;
const LANG_NEUTRAL = 0;
      LANG_ENGLISH = 9;
var nameptr,ptr : PResInfoNode;
begin
  FindResourceExSingleFile:=0;
  precision:=-1;
  nameptr:=InternalFindResource(ResHeader,ResourceName,ResourceType);
  if nameptr=nil then exit;

  precision:=4;
  //try exact match
  ptr:=FindSubLanguage(ResHeader,nameptr,Language,$FFFF);
  //try primary language
  if ptr=nil then
  begin
    dec(precision);
    ptr:=FindSubLanguage(ResHeader,nameptr,Language,$3FF);
  end;
  //try language neutral
  if ptr=nil then
  begin
    dec(precision);
    ptr:=FindSubLanguage(ResHeader,nameptr,LANG_NEUTRAL,$3FF);
  end;
  //try english
  if ptr=nil then
  begin
    dec(precision);
    ptr:=FindSubLanguage(ResHeader,nameptr,LANG_ENGLISH,$3FF);
  end;
  //nothing found, return the first one
  if ptr=nil then
  begin
    dec(precision);
    ptr:=GetResInfoPtr(ResHeader,nameptr^.subptr);
  end;

  if ptr^.ncounthandle=0 then
  begin
    reshandles[usedhandles]:=ptr;
    inc(usedhandles);
    ptr^.ncounthandle:=usedhandles;
  end;
  FindResourceExSingleFile:=ptr^.ncounthandle;
end;

function EnumResourceTypesSingleFile(ResHeader,Other : PExtHeader; ModuleHandle
 : TFPResourceHMODULE; EnumFunc : EnumResTypeProc; lParam : PtrInt) : LongBool;
var ptr,otarr : PResInfoNode;
    totn, totid, ottotn, ottotid, i : longword;
    pc : pchar;
begin
  EnumResourceTypesSingleFile:=false;
  if ResHeader=nil then exit;

  ptr:=GetResInfoPtr(Resheader,sizeof(TExtHeader));
  totn:=ptr^.ncounthandle;
  totid:=totn+ptr^.idcountsize;
  ptr:=GetResInfoPtr(Resheader,ptr^.subptr);

  if Other<>nil then
  begin
    otarr:=GetResInfoPtr(Other,sizeof(TExtHeader));
    ottotn:=otarr^.ncounthandle;
    ottotid:=ottotn+otarr^.idcountsize-1;
    otarr:=GetResInfoPtr(Other,otarr^.subptr)
  end;

  EnumResourceTypesSingleFile:=true;
  i:=0;
  while i<totn do //named entries
  begin
    pc:=GetPChar(Resheader,ptr[i].nameid);
    if (Other=nil) or (BinSearchStr(Other,otarr,pc,0,ottotn-1)=nil) then
      if not EnumFunc(ModuleHandle,pc,lParam) then exit;
    inc(i);
  end;
  while i<totid do
  begin
    if (Other=nil) or (BinSearchInt(otarr,PChar(ptr[i].nameid),ottotn,ottotid)=nil) then
      if not EnumFunc(ModuleHandle,PChar(ptr[i].nameid),lParam) then exit;
    inc(i);
  end;
end;

function EnumResourceNamesSingleFile(ResHeader,Other : PExtHeader;
  ModuleHandle : TFPResourceHMODULE; ResourceType : PChar;
  EnumFunc : EnumResNameProc; lParam : PtrInt) : LongBool;
var ptr,otarr : PResInfoNode;
    totn, totid, ottotn, ottotid, i : longword;
    pc : pchar;
begin
  EnumResourceNamesSingleFile:=False;
  if ResHeader=nil then exit;

  ptr:=GetResInfoPtr(ResHeader,sizeof(TExtHeader));
  ptr:=BinSearchRes(ResHeader,ptr,ResourceType);
  if ptr=nil then exit;
  totn:=ptr^.ncounthandle;
  totid:=totn+ptr^.idcountsize;
  ptr:=GetResInfoPtr(ResHeader,ptr^.subptr);

  if Other<>nil then
  begin
    otarr:=GetResInfoPtr(Other,sizeof(TExtHeader));
    otarr:=BinSearchRes(Other,otarr,ResourceType);
    if otarr<>nil then
    begin
      ottotn:=otarr^.ncounthandle;
      ottotid:=ottotn+otarr^.idcountsize-1;
      otarr:=GetResInfoPtr(Other,otarr^.subptr)
    end;
  end
  else otarr:=nil;

  EnumResourceNamesSingleFile:=true;
  i:=0;
  while i<totn do //named entries
  begin
    pc:=GetPChar(ResHeader,ptr[i].nameid);
    if (otarr=nil) or (BinSearchStr(Other,otarr,pc,0,ottotn-1)=nil) then
      if not EnumFunc(ModuleHandle,ResourceType,pc,lParam) then exit;
    inc(i);
  end;
  while i<totid do
  begin
    if (otarr=nil) or (BinSearchInt(otarr,PChar(ptr[i].nameid),ottotn,ottotid)=nil) then
      if not EnumFunc(ModuleHandle,ResourceType,PChar(ptr[i].nameid),lParam) then exit;
    inc(i);
  end;
end;

function EnumResourceLanguagesSingleFile(ResHeader,Other : PExtHeader;
  ModuleHandle : TFPResourceHMODULE; ResourceType, ResourceName : PChar;
  EnumFunc : EnumResLangProc; lParam : PtrInt) : LongBool;
var ptr, otarr : PResInfoNode;
    tot, ottot, i : integer;
begin
  EnumResourceLanguagesSingleFile:=False;
  ptr:=InternalFindResource(ResHeader,ResourceName,ResourceType);
  if ptr=nil then exit;

  tot:=ptr^.idcountsize;
  ptr:=GetResInfoPtr(ResHeader,ptr^.subptr);
  
  if Other<>nil then
  begin
    otarr:=InternalFindResource(Other,ResourceName,ResourceType);
    if otarr<>nil then
    begin
      ottot:=otarr^.idcountsize-1;
      otarr:=GetResInfoPtr(Other,otarr^.subptr)
    end;
  end
  else otarr:=nil;

  EnumResourceLanguagesSingleFile:=true;
  i:=0;
  while i<tot do
  begin
    if (otarr=nil) or (BinSearchInt(otarr,PChar(ptr[i].nameid),0,ottot)=nil) then
      if not EnumFunc(ModuleHandle,ResourceType,ResourceName,PtrUInt(
        ptr[i].nameid),lParam) then exit;
    inc(i);
  end;
end;

(*****************************************************************************
                             Public Resource Functions
*****************************************************************************)

function ExtHINSTANCE : TFPResourceHMODULE;
begin
  ExtHINSTANCE:=0;
end;

function ExtEnumResourceTypes(ModuleHandle : TFPResourceHMODULE; EnumFunc : EnumResTypeProc; lParam : PtrInt) : LongBool;
begin
  ExtEnumResourceTypes:=false;
  if EnumResourceTypesSingleFile(ResFileInfoArch.Resheader,nil,ModuleHandle,
    EnumFunc,lParam) then ExtEnumResourceTypes:=true;
  if EnumResourceTypesSingleFile(ResFileInfo.Resheader,
    ResFileInfoArch.Resheader,ModuleHandle,EnumFunc,lParam) then ExtEnumResourceTypes:=true;
end;

function ExtEnumResourceNames(ModuleHandle : TFPResourceHMODULE; ResourceType : PChar; EnumFunc : EnumResNameProc; lParam : PtrInt) : LongBool;
begin
  ExtEnumResourceNames:=False;
  if EnumResourceNamesSingleFile(ResFileInfoArch.Resheader,nil,ModuleHandle,
    ResourceType,EnumFunc,lParam) then ExtEnumResourceNames:=true;
  if EnumResourceNamesSingleFile(ResFileInfo.Resheader,
    ResFileInfoArch.Resheader,ModuleHandle,ResourceType,EnumFunc,lParam) then ExtEnumResourceNames:=true;
end;

function ExtEnumResourceLanguages(ModuleHandle : TFPResourceHMODULE; ResourceType, ResourceName : PChar; EnumFunc : EnumResLangProc; lParam : PtrInt) : LongBool;
begin
  ExtEnumResourceLanguages:=False;
  if EnumResourceLanguagesSingleFile(ResFileInfoArch.Resheader,nil,ModuleHandle,
    ResourceType,ResourceName,EnumFunc,lParam) then ExtEnumResourceLanguages:=true;
  if EnumResourceLanguagesSingleFile(ResFileInfo.Resheader,
    ResFileInfoArch.Resheader,ModuleHandle,ResourceType,ResourceName,EnumFunc,
    lParam) then ExtEnumResourceLanguages:=true;
end;

function ExtFindResource(ModuleHandle: TFPResourceHMODULE; ResourceName, ResourceType: PChar): TFPResourceHandle;
begin
  //search for resource in architecture-dependent res file first
  ExtFindResource:=FindResourceSingleFile(ResFileInfoArch.ResHeader,ResourceName,ResourceType);
  if ExtFindResource=0 then
    ExtFindResource:=FindResourceSingleFile(ResFileInfo.ResHeader,ResourceName,ResourceType);
end;

function ExtFindResourceEx(ModuleHandle: TFPResourceHMODULE; ResourceType, ResourceName: PChar; Language : word): TFPResourceHandle;
var precar, precsh : integer;
    handlear, handlesh : TResourceHandle;
begin
  //architecture-dependent res file
  handlear:=FindResourceExSingleFile(ResFileInfoArch.ResHeader,ResourceType,
    ResourceName,Language,precar);
  //architecture-independent res file
  handlesh:=FindResourceExSingleFile(ResFileInfo.ResHeader,ResourceType,
    ResourceName,Language,precsh);

  //return architecture-independent resource only if its language id is closer
  //to the one user asked for
  if precsh>precar then ExtFindResourceEx:=handlesh
  else ExtFindResourceEx:=handlear;
end;

function ExtLoadResource(ModuleHandle: TFPResourceHMODULE; ResHandle: TFPResourceHandle): TFPResourceHGLOBAL;
var ptr : PResInfoNode;
    base : PExtHeader;
begin
  ExtLoadResource:=0;
  if (ResHandle<=0) or (ResHandle>usedhandles) then exit;
  ptr:=PResInfoNode(reshandles[ResHandle-1]);
  base:=ResFileInfoArch.ResHeader;
  //if ptr isn't in architecture-dependent file memory area...
  if (base=nil) or (pointer(ptr)<=pointer(base))
    or (pointer(ptr)>=GetPtr(base,base^.hdrsize)) then
      base:=ResFileInfo.ResHeader;
  ExtLoadResource:=TFPResourceHGLOBAL(GetPtr(base,ptr^.subptr));
end;

function ExtSizeofResource(ModuleHandle: TFPResourceHMODULE; ResHandle: TFPResourceHandle): LongWord;
begin
  ExtSizeofResource:=0;
  if (ResHandle<=0) or (ResHandle>usedhandles) then exit;
  ExtSizeofResource:=PResInfoNode(reshandles[ResHandle-1])^.idcountsize;
end;

function ExtLockResource(ResData: TFPResourceHGLOBAL): Pointer;
begin
  ExtLockResource:=Nil;
  if (ResFileInfoArch.Resheader=nil) and (ResFileInfo.Resheader=nil) then exit;
  ExtLockResource:=Pointer(ResData);
end;

function ExtUnlockResource(ResData: TFPResourceHGLOBAL): LongBool;
begin
  ExtUnlockResource:=(ResFileInfoArch.Resheader<>nil) or (ResFileInfo.Resheader<>nil);
end;

function ExtFreeResource(ResData: TFPResourceHGLOBAL): LongBool;
begin
  ExtFreeResource:=(ResFileInfoArch.Resheader<>nil) or (ResFileInfo.Resheader<>nil);
end;

const
  ExternalResourceManager : TResourceManager =
  (
    HINSTANCEFunc : @ExtHINSTANCE;
    EnumResourceTypesFunc : @ExtEnumResourceTypes;
    EnumResourceNamesFunc : @ExtEnumResourceNames;
    EnumResourceLanguagesFunc : @ExtEnumResourceLanguages;
    FindResourceFunc : @ExtFindResource;
    FindResourceExFunc : @ExtFindResourceEx;
    LoadResourceFunc : @ExtLoadResource;
    SizeofResourceFunc : @ExtSizeofResource;
    LockResourceFunc : @ExtLockResource;
    UnlockResourceFunc : @ExtUnlockResource;
    FreeResourceFunc : @ExtFreeResource;
  );
