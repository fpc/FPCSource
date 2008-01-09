{
    This file is part of the Free Pascal run time library.
    Copyright (c) 2008 by Giulio Bernardi

    Resource support as external files

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}
{
  This file implements two kinds of external resource support:
   - one for systems that support the mmap call (usually unix-like oses)
   - one fallback implementation based on pascal files and GetMem/FreeMem
   
  Be sure to define EXTRES_MMAP or EXTRES_GENERIC before including this file!
}
{$IF defined(EXTRES_MMAP) and defined(EXTRES_GENERIC)}
{$FATAL EXTRES_MMAP and EXTRES_GENERIC can't be defined together}
{$ENDIF}

{$IF (not defined(EXTRES_MMAP)) and (not defined(EXTRES_GENERIC))}
{$FATAL EXTRES_MMAP or EXTRES_GENERIC must be defined}
{$ENDIF}

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

  {$IFDEF EXTRES_GENERIC}
  TResHandle = record
    info : PResInfoNode;
    ptr : Pointer;
  end;
  PResHandle = ^TResHandle;
  {$ENDIF}

var ResHeader : PExtHeader = nil;
    usedhandles : longword = 0;
    {$IFDEF EXTRES_MMAP}
    fd : integer;
    fd_size : longword;
    reshandles : PPointer = nil;
    {$ENDIF}
    {$IFDEF EXTRES_GENERIC}
    fd : file;
    reshandles : PResHandle = nil;
    {$ENDIF}

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

function GetResInfoPtr(const offset : longword) : PResInfoNode; inline;
begin
  GetResInfoPtr:=PResInfoNode(PtrUInt(ResHeader)+offset);
end;

function GetPchar(const offset : longword) : Pchar; inline;
begin
  GetPchar:=Pchar(PtrUInt(ResHeader)+offset);
end;

function GetPtr(const offset : longword) : Pointer; inline;
begin
  GetPtr:=Pointer(PtrUInt(ResHeader)+offset);
end;

procedure FixResEndian;
var ptr : plongword;
    blockend : plongword;
begin
  //all info nodes reside in a contiguos block of memory.
  //they are all 16 bytes long and made by longwords
  //so, simply swap each longword in the block
  ptr:=GetPtr(sizeof(TExtHeader));
  blockend:=GetPtr(ResHeader^.nodesize);
  while ptr<blockend do
  begin
    ptr^:=SwapEndian(ptr^);
    inc(ptr);
  end;
end;

function GetExtResPath : pchar;
var len, i : integer;
    pathstr : shortstring;
begin
  pathstr:=paramstr(0);
  len:=byte(pathstr[0]);
  i:=len;
  //writeln('exe name is ',pathstr);
  //find position of extension
  while (i>0) and (not (pathstr[i] in ['.',DirectorySeparator])) do
    dec(i);
  if (i>0) and (pathstr[i]='.') then dec(i)
  else i:=len;
  pathstr[0]:=Chr(i);
  pathstr:=pathstr+FPCRES_EXT;
  len:=byte(pathstr[0]);
  GetExtResPath:=GetMem(len+1);
  Move(pathstr[1],GetExtResPath[0],len);
  GetExtResPath[len]:=#0;
  //writeln('Resource file is ',GetExtResPath);
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
    resstr:=GetPchar(arr[pivot].nameid);
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

function BinSearchRes(root : PResInfoNode; aDesc : PChar) : PResInfoNode;
var aID : PtrUint;
begin
  if InternalIsIntResource(aDesc,aID) then
    BinSearchRes:=BinSearchInt(GetResInfoPtr(root^.subptr),PChar(aID),
      root^.ncounthandle,root^.ncounthandle+root^.idcountsize-1)
  else
    BinSearchRes:=BinSearchStr(GetResInfoPtr(root^.subptr),aDesc,0,
      root^.ncounthandle-1);
end;

//Returns a pointer to a name node.
function InternalFindResource(ResourceName, ResourceType: PChar):
 PResInfoNode;
begin
  InternalFindResource:=nil;
  if ResHeader=nil then exit;
  InternalFindResource:=GetResInfoPtr(sizeof(TExtHeader));

  InternalFindResource:=BinSearchRes(InternalFindResource,ResourceType);
  if InternalFindResource<>nil then
    InternalFindResource:=BinSearchRes(InternalFindResource,ResourceName);
end;

function FindSubLanguage(aPtr : PResInfoNode; aLangID : word; aMask: word) : PResInfoNode;
var arr : PResInfoNode;
    i : longword;
begin
  FindSubLanguage:=nil;
  arr:=GetResInfoPtr(aPtr^.subptr);
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

{$IFDEF EXTRES_MMAP}
procedure InitResources;
const
  PROT_READ  = 1;
  PROT_WRITE = 2;
var respath : pchar;
    fdstat : stat;
begin
  respath:=GetExtResPath;
//  writeln('respath ',respath);
  fd:=FpOpen(respath,O_RDONLY,0);
//  writeln('fpopen returned ',fd);
  FreeMem(respath);
  if fd=-1 then exit;
  if FpFStat(fd,fdstat)<>0 then
  begin
//    writeln('fpfstat failed');
    FpClose(fd);
    exit;
  end;
//  writeln('fpfstat suceeded');
  fd_size:=fdstat.st_size;
  ResHeader:=PExtHeader(Fpmmap(nil,fd_size,PROT_READ or PROT_WRITE,
    MAP_PRIVATE,fd,0));
//  writeln('fpmmap returned ',PtrInt(ResHeader));
  if PtrInt(ResHeader)=-1 then
  begin
    FpClose(fd);
    exit;
  end;
  if (ResHeader^.magic<>FPCRES_MAGIC) or
    (ResHeader^.version<>fpcres_version) then
  begin
    FpClose(fd);
    exit;
  end;
//  writeln('magic ok');
  if ResHeader^.endianess<>FPCRES_ENDIAN then
  begin
    ResHeader^.count:=SwapEndian(ResHeader^.count);
    ResHeader^.nodesize:=SwapEndian(ResHeader^.nodesize);
    ResHeader^.hdrsize:=SwapEndian(ResHeader^.hdrsize);
    FixResEndian;
  end;
  reshandles:=GetMem(sizeof(Pointer)*ResHeader^.count);
  FillByte(reshandles^,sizeof(Pointer)*ResHeader^.count,0);
end;

procedure FinalizeResources;
begin
  if ResHeader=nil then exit;
  FreeMem(reshandles);
  Fpmunmap(ResHeader,fd_size);
  FpClose(fd);
end;
{$ENDIF}

{$IFDEF EXTRES_GENERIC}
procedure InitResources;
var respath : pchar;
    tmp : longword;
    tmpptr : pbyte;
label ExitErrMem, ExitErrFile, ExitNoErr;
begin
  respath:=GetExtResPath;
//  writeln('respath ',respath);
  Assign(fd,respath);
  FreeMem(respath);
  {$I-}
  Reset(fd,1);
  {$I+}
  if IOResult<>0 then exit;
//  writeln('file opened');
  ResHeader:=GetMem(sizeof(TExtHeader));
  if ResHeader=nil then goto ExitErrFile;
  {$I-}
  BlockRead(fd,ResHeader^,sizeof(TExtHeader),tmp);
  {$I+}
  if (IOResult<>0) or (tmp<>sizeof(TExtHeader)) then goto ExitErrMem;
  if (ResHeader^.magic<>FPCRES_MAGIC) or (ResHeader^.version<>fpcres_version)
    then goto ExitErrMem;
//  writeln('magic ok');
  if ResHeader^.endianess<>FPCRES_ENDIAN then
  begin
    ResHeader^.count:=SwapEndian(ResHeader^.count);
    ResHeader^.nodesize:=SwapEndian(ResHeader^.nodesize);
    ResHeader^.hdrsize:=SwapEndian(ResHeader^.hdrsize);
  end;
  SysReallocMem(ResHeader,ResHeader^.hdrsize);
  if ResHeader=nil then goto ExitErrFile;
  tmpptr:=pbyte(ResHeader);
  inc(tmpptr,sizeof(TExtHeader));
  {$I-}
  BlockRead(fd,tmpptr^,ResHeader^.hdrsize-sizeof(TExtHeader),tmp);
  {$I+}
  if (IOResult<>0) or (tmp<>ResHeader^.hdrsize-sizeof(TExtHeader)) then goto ExitErrMem;
  if ResHeader^.endianess<>FPCRES_ENDIAN then
    FixResEndian;
  reshandles:=GetMem(sizeof(TResHandle)*ResHeader^.count);
  FillByte(reshandles^,sizeof(TResHandle)*ResHeader^.count,0);
  goto ExitNoErr;

  ExitErrMem:
    FreeMem(ResHeader);
    ResHeader:=nil;
  ExitErrFile:
    {$I-}
    Close(fd);
    {$I+}
  ExitNoErr:
end;

procedure FinalizeResources;
begin
  if ResHeader=nil then exit;
  FreeMem(reshandles);
  FreeMem(ResHeader);
  Close(fd);
end;
{$ENDIF}

(*****************************************************************************
                             Public Resource Functions
*****************************************************************************)

Function ExtHINSTANCE : TFPResourceHMODULE;
begin
  ExtHINSTANCE:=0;
end;

function ExtEnumResourceTypes(ModuleHandle : TFPResourceHMODULE; EnumFunc : EnumResTypeProc; lParam : PtrInt) : LongBool;
var ptr : PResInfoNode;
    totn, totid, i : longword;
    pc : pchar;
begin
  ExtEnumResourceTypes:=False;
  if ResHeader=nil then exit;
  ptr:=GetResInfoPtr(sizeof(TExtHeader));
  totn:=ptr^.ncounthandle;
  totid:=totn+ptr^.idcountsize;
  ptr:=GetResInfoPtr(ptr^.subptr);
  ExtEnumResourceTypes:=true;
  i:=0;
  while i<totn do //named entries
  begin
    pc:=GetPChar(ptr[i].nameid);
    if not EnumFunc(ModuleHandle,pc,lParam) then exit;
    inc(i);
  end;
  while i<totid do
  begin
    if not EnumFunc(ModuleHandle,PChar(ptr[i].nameid),lParam) then exit;
    inc(i);
  end;
end;

function ExtEnumResourceNames(ModuleHandle : TFPResourceHMODULE; ResourceType : PChar; EnumFunc : EnumResNameProc; lParam : PtrInt) : LongBool;
var ptr : PResInfoNode;
    totn, totid, i : longword;
    pc : pchar;
begin
  ExtEnumResourceNames:=False;
  if ResHeader=nil then exit;
  ptr:=GetResInfoPtr(sizeof(TExtHeader));

  ptr:=BinSearchRes(ptr,ResourceType);
  if ptr=nil then exit;

  totn:=ptr^.ncounthandle;
  totid:=totn+ptr^.idcountsize;
  ptr:=GetResInfoPtr(ptr^.subptr);
  ExtEnumResourceNames:=true;
  i:=0;
  while i<totn do //named entries
  begin
    pc:=GetPChar(ptr[i].nameid);
    if not EnumFunc(ModuleHandle,ResourceType,pc,lParam) then exit;
    inc(i);
  end;
  while i<totid do
  begin
    if not EnumFunc(ModuleHandle,ResourceType,PChar(ptr[i].nameid),lParam) then exit;
    inc(i);
  end;
end;

function ExtEnumResourceLanguages(ModuleHandle : TFPResourceHMODULE; ResourceType, ResourceName : PChar; EnumFunc : EnumResLangProc; lParam : PtrInt) : LongBool;
var ptr : PResInfoNode;
    tot, i : integer;
begin
  ExtEnumResourceLanguages:=False;
  ptr:=InternalFindResource(ResourceName,ResourceType);
  if ptr=nil then exit;

  tot:=ptr^.idcountsize;
  ptr:=GetResInfoPtr(ptr^.subptr);
  ExtEnumResourceLanguages:=true;
  i:=0;
  while i<tot do
  begin
    if not EnumFunc(ModuleHandle,ResourceType,ResourceName,PtrUInt(ptr[i].nameid),lParam) then exit;
    inc(i);
  end;
end;

Function ExtFindResource(ModuleHandle: TFPResourceHMODULE; ResourceName, ResourceType: PChar)
: TFPResourceHandle;
var ptr : PResInfoNode;
begin
  ExtFindResource:=0;
  ptr:=InternalFindResource(ResourceName,ResourceType);
  if ptr=nil then exit;

  //first language id
  ptr:=GetResInfoPtr(ptr^.subptr);
  if ptr^.ncounthandle=0 then
  begin
    {$IFDEF EXTRES_MMAP}
    reshandles[usedhandles]:=ptr;
    {$ENDIF}
    {$IFDEF EXTRES_GENERIC}
    reshandles[usedhandles].info:=ptr;
    {$ENDIF}
    inc(usedhandles);
    ptr^.ncounthandle:=usedhandles;
  end;
  ExtFindResource:=ptr^.ncounthandle;
end;

Function ExtFindResourceEx(ModuleHandle: TFPResourceHMODULE; ResourceType,
  ResourceName: PChar; Language : word): TFPResourceHandle;
const LANG_NEUTRAL = 0;
      LANG_ENGLISH = 9;
var nameptr,ptr : PResInfoNode;
begin
  ExtFindResourceEx:=0;
  nameptr:=InternalFindResource(ResourceName,ResourceType);
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
    ptr:=GetResInfoPtr(nameptr^.subptr);

  if ptr^.ncounthandle=0 then
  begin
    {$IFDEF EXTRES_MMAP}
    reshandles[usedhandles]:=ptr;
    {$ENDIF}
    {$IFDEF EXTRES_GENERIC}
    reshandles[usedhandles].info:=ptr;
    {$ENDIF}
    inc(usedhandles);
    ptr^.ncounthandle:=usedhandles;
  end;
  ExtFindResourceEx:=ptr^.ncounthandle;
end;

{$IFDEF EXTRES_MMAP}
Function ExtLoadResource(ModuleHandle: TFPResourceHMODULE; ResHandle: TFPResourceHandle): TFPResourceHGLOBAL;
begin
  ExtLoadResource:=0;
  if ResHeader=nil then exit;
  if (ResHandle<=0) or (ResHandle>usedhandles) then exit;
  ExtLoadResource:=TFPResourceHGLOBAL(GetPtr(PResInfoNode(reshandles[ResHandle-1])^.subptr));
end;

Function ExtFreeResource(ResData: TFPResourceHGLOBAL): LongBool;
begin
  ExtFreeResource:=(ResHeader<>nil);
end;

Function ExtSizeofResource(ModuleHandle: TFPResourceHMODULE; ResHandle: TFPResourceHandle): LongWord;
begin
  ExtSizeofResource:=0;
  if ResHeader=nil then exit;
  if (ResHandle<=0) or (ResHandle>usedhandles) then exit;
  ExtSizeofResource:=PResInfoNode(reshandles[ResHandle-1])^.idcountsize;
end;
{$ENDIF}

{$IFDEF EXTRES_GENERIC}
(*
Resource data memory layout:
-2*sizeof(pointer)  Reference count
  -sizeof(pointer)  Pointer to resource info
                 0  Resource data
*)
Function ExtLoadResource(ModuleHandle: TFPResourceHMODULE; ResHandle: TFPResourceHandle): TFPResourceHGLOBAL;
var ptr : PPtrUInt;
    tmp : longword;
begin
  ExtLoadResource:=0;
  if ResHeader=nil then exit;
  if (ResHandle<=0) or (ResHandle>usedhandles) then exit;

  if reshandles[ResHandle-1].ptr=nil then
  begin
    {$I-}
    Seek(fd,reshandles[ResHandle-1].info^.subptr);
    {$I+}
    if IOResult<>0 then exit;
    ptr:=GetMem(reshandles[ResHandle-1].info^.idcountsize+2*sizeof(PtrUint));
    if ptr=nil then exit;
    ptr^:=1; //refcount
    inc(ptr);
    ptr^:=PtrUInt(reshandles[ResHandle-1].info); //ptr to resource info
    inc(ptr);
    {$I-}
    BlockRead(fd,ptr^,reshandles[ResHandle-1].info^.idcountsize,tmp);
    {$I+}
    if (IOResult<>0) or (tmp<>reshandles[ResHandle-1].info^.idcountsize) then
    begin
      FreeMem(ptr);
      exit;
    end;
    reshandles[ResHandle-1].ptr:=ptr;
  end
  else
  begin
    ptr:=reshandles[ResHandle-1].ptr;
    dec(ptr,2);
    inc(ptr^,1); //increase reference count
  end;
  ExtLoadResource:=TFPResourceHGLOBAL(reshandles[ResHandle-1].ptr);
end;

Function ExtFreeResource(ResData: TFPResourceHGLOBAL): LongBool;
var ptrinfo : PResInfoNode;
    ptr : PPtrUInt;
begin
  ExtFreeResource:=(ResHeader<>nil);
  if not ExtFreeResource then exit;
  ptr:=PPtrUInt(ResData);
  dec(ptr,2);
  dec(ptr^); //decrease reference count
  if ptr^=0 then
  begin
    inc(ptr);
    ptrinfo:=PResInfoNode(ptr^);
    dec(ptr);
    FreeMem(ptr);
    reshandles[ptrinfo^.ncounthandle-1].ptr:=nil;
  end;
  ExtFreeResource:=true;
end;

Function ExtSizeofResource(ModuleHandle: TFPResourceHMODULE; ResHandle: TFPResourceHandle): LongWord;
var ptrinfo : PResInfoNode;
begin
  ExtSizeofResource:=0;
  if ResHeader=nil then exit;
  if (ResHandle<=0) or (ResHandle>usedhandles) then exit;
  ptrinfo:=PResInfoNode(reshandles[ResHandle-1].info);
  ExtSizeofResource:=ptrinfo^.idcountsize;
end;
{$ENDIF}

Function ExtLockResource(ResData: TFPResourceHGLOBAL): Pointer;
begin
  ExtLockResource:=Nil;
  if ResHeader=nil then exit;
  ExtLockResource:=Pointer(ResData);
end;

Function ExtUnlockResource(ResData: TFPResourceHGLOBAL): LongBool;
begin
  ExtUnlockResource:=(ResHeader<>nil);
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

{
    This file is part of the Free Pascal run time library.
    Copyright (c) 2008 by Giulio Bernardi

    Resource support as external files

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}
{
  This file implements two kinds of external resource support:
   - one for systems that support the mmap call (usually unix-like oses)
   - one fallback implementation based on pascal files and GetMem/FreeMem
   
  Be sure to define EXTRES_MMAP or EXTRES_GENERIC before including this file!
}
{$IF defined(EXTRES_MMAP) and defined(EXTRES_GENERIC)}
{$FATAL EXTRES_MMAP and EXTRES_GENERIC can't be defined together}
{$ENDIF}

{$IF (not defined(EXTRES_MMAP)) and (not defined(EXTRES_GENERIC))}
{$FATAL EXTRES_MMAP or EXTRES_GENERIC must be defined}
{$ENDIF}

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

  {$IFDEF EXTRES_GENERIC}
  TResHandle = record
    info : PResInfoNode;
    ptr : Pointer;
  end;
  PResHandle = ^TResHandle;
  {$ENDIF}

var ResHeader : PExtHeader = nil;
    usedhandles : longword = 0;
    {$IFDEF EXTRES_MMAP}
    fd : integer;
    fd_size : longword;
    reshandles : PPointer = nil;
    {$ENDIF}
    {$IFDEF EXTRES_GENERIC}
    fd : file;
    reshandles : PResHandle = nil;
    {$ENDIF}

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

function GetResInfoPtr(const offset : longword) : PResInfoNode; inline;
begin
  GetResInfoPtr:=PResInfoNode(PtrUInt(ResHeader)+offset);
end;

function GetPchar(const offset : longword) : Pchar; inline;
begin
  GetPchar:=Pchar(PtrUInt(ResHeader)+offset);
end;

function GetPtr(const offset : longword) : Pointer; inline;
begin
  GetPtr:=Pointer(PtrUInt(ResHeader)+offset);
end;

procedure FixResEndian;
var ptr : plongword;
    blockend : plongword;
begin
  //all info nodes reside in a contiguos block of memory.
  //they are all 16 bytes long and made by longwords
  //so, simply swap each longword in the block
  ptr:=GetPtr(sizeof(TExtHeader));
  blockend:=GetPtr(ResHeader^.nodesize);
  while ptr<blockend do
  begin
    ptr^:=SwapEndian(ptr^);
    inc(ptr);
  end;
end;

function GetExtResPath : pchar;
var len, i : integer;
    pathstr : shortstring;
begin
  pathstr:=paramstr(0);
  len:=byte(pathstr[0]);
  i:=len;
  //writeln('exe name is ',pathstr);
  //find position of extension
  while (i>0) and (not (pathstr[i] in ['.',DirectorySeparator])) do
    dec(i);
  if (i>0) and (pathstr[i]='.') then dec(i)
  else i:=len;
  pathstr[0]:=Chr(i);
  pathstr:=pathstr+FPCRES_EXT;
  len:=byte(pathstr[0]);
  GetExtResPath:=GetMem(len+1);
  Move(pathstr[1],GetExtResPath[0],len);
  GetExtResPath[len]:=#0;
  //writeln('Resource file is ',GetExtResPath);
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
    resstr:=GetPchar(arr[pivot].nameid);
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

function BinSearchRes(root : PResInfoNode; aDesc : PChar) : PResInfoNode;
var aID : PtrUint;
begin
  if InternalIsIntResource(aDesc,aID) then
    BinSearchRes:=BinSearchInt(GetResInfoPtr(root^.subptr),PChar(aID),
      root^.ncounthandle,root^.ncounthandle+root^.idcountsize-1)
  else
    BinSearchRes:=BinSearchStr(GetResInfoPtr(root^.subptr),aDesc,0,
      root^.ncounthandle-1);
end;

//Returns a pointer to a name node.
function InternalFindResource(ResourceName, ResourceType: PChar):
 PResInfoNode;
begin
  InternalFindResource:=nil;
  if ResHeader=nil then exit;
  InternalFindResource:=GetResInfoPtr(sizeof(TExtHeader));

  InternalFindResource:=BinSearchRes(InternalFindResource,ResourceType);
  if InternalFindResource<>nil then
    InternalFindResource:=BinSearchRes(InternalFindResource,ResourceName);
end;

function FindSubLanguage(aPtr : PResInfoNode; aLangID : word; aMask: word) : PResInfoNode;
var arr : PResInfoNode;
    i : longword;
begin
  FindSubLanguage:=nil;
  arr:=GetResInfoPtr(aPtr^.subptr);
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

{$IFDEF EXTRES_MMAP}
procedure InitResources;
const
  PROT_READ  = 1;
  PROT_WRITE = 2;
var respath : pchar;
    fdstat : stat;
begin
  respath:=GetExtResPath;
//  writeln('respath ',respath);
  fd:=FpOpen(respath,O_RDONLY,0);
//  writeln('fpopen returned ',fd);
  FreeMem(respath);
  if fd=-1 then exit;
  if FpFStat(fd,fdstat)<>0 then
  begin
//    writeln('fpfstat failed');
    FpClose(fd);
    exit;
  end;
//  writeln('fpfstat suceeded');
  fd_size:=fdstat.st_size;
  ResHeader:=PExtHeader(Fpmmap(nil,fd_size,PROT_READ or PROT_WRITE,
    MAP_PRIVATE,fd,0));
//  writeln('fpmmap returned ',PtrInt(ResHeader));
  if PtrInt(ResHeader)=-1 then
  begin
    FpClose(fd);
    exit;
  end;
  if (ResHeader^.magic<>FPCRES_MAGIC) or
    (ResHeader^.version<>fpcres_version) then
  begin
    FpClose(fd);
    exit;
  end;
//  writeln('magic ok');
  if ResHeader^.endianess<>FPCRES_ENDIAN then
  begin
    ResHeader^.count:=SwapEndian(ResHeader^.count);
    ResHeader^.nodesize:=SwapEndian(ResHeader^.nodesize);
    ResHeader^.hdrsize:=SwapEndian(ResHeader^.hdrsize);
    FixResEndian;
  end;
  reshandles:=GetMem(sizeof(Pointer)*ResHeader^.count);
  FillByte(reshandles^,sizeof(Pointer)*ResHeader^.count,0);
end;

procedure FinalizeResources;
begin
  if ResHeader=nil then exit;
  FreeMem(reshandles);
  Fpmunmap(ResHeader,fd_size);
  FpClose(fd);
end;
{$ENDIF}

{$IFDEF EXTRES_GENERIC}
procedure InitResources;
var respath : pchar;
    tmp : longword;
    tmpptr : pbyte;
label ExitErrMem, ExitErrFile, ExitNoErr;
begin
  respath:=GetExtResPath;
//  writeln('respath ',respath);
  Assign(fd,respath);
  FreeMem(respath);
  {$I-}
  Reset(fd,1);
  {$I+}
  if IOResult<>0 then exit;
//  writeln('file opened');
  ResHeader:=GetMem(sizeof(TExtHeader));
  if ResHeader=nil then goto ExitErrFile;
  {$I-}
  BlockRead(fd,ResHeader^,sizeof(TExtHeader),tmp);
  {$I+}
  if (IOResult<>0) or (tmp<>sizeof(TExtHeader)) then goto ExitErrMem;
  if (ResHeader^.magic<>FPCRES_MAGIC) or (ResHeader^.version<>fpcres_version)
    then goto ExitErrMem;
//  writeln('magic ok');
  if ResHeader^.endianess<>FPCRES_ENDIAN then
  begin
    ResHeader^.count:=SwapEndian(ResHeader^.count);
    ResHeader^.nodesize:=SwapEndian(ResHeader^.nodesize);
    ResHeader^.hdrsize:=SwapEndian(ResHeader^.hdrsize);
  end;
  SysReallocMem(ResHeader,ResHeader^.hdrsize);
  if ResHeader=nil then goto ExitErrFile;
  tmpptr:=pbyte(ResHeader);
  inc(tmpptr,sizeof(TExtHeader));
  {$I-}
  BlockRead(fd,tmpptr^,ResHeader^.hdrsize-sizeof(TExtHeader),tmp);
  {$I+}
  if (IOResult<>0) or (tmp<>ResHeader^.hdrsize-sizeof(TExtHeader)) then goto ExitErrMem;
  if ResHeader^.endianess<>FPCRES_ENDIAN then
    FixResEndian;
  reshandles:=GetMem(sizeof(TResHandle)*ResHeader^.count);
  FillByte(reshandles^,sizeof(TResHandle)*ResHeader^.count,0);
  goto ExitNoErr;

  ExitErrMem:
    FreeMem(ResHeader);
    ResHeader:=nil;
  ExitErrFile:
    {$I-}
    Close(fd);
    {$I+}
  ExitNoErr:
end;

procedure FinalizeResources;
begin
  if ResHeader=nil then exit;
  FreeMem(reshandles);
  FreeMem(ResHeader);
  Close(fd);
end;
{$ENDIF}

(*****************************************************************************
                             Public Resource Functions
*****************************************************************************)

Function ExtHINSTANCE : TFPResourceHMODULE;
begin
  ExtHINSTANCE:=0;
end;

function ExtEnumResourceTypes(ModuleHandle : TFPResourceHMODULE; EnumFunc : EnumResTypeProc; lParam : PtrInt) : LongBool;
var ptr : PResInfoNode;
    totn, totid, i : longword;
    pc : pchar;
begin
  ExtEnumResourceTypes:=False;
  if ResHeader=nil then exit;
  ptr:=GetResInfoPtr(sizeof(TExtHeader));
  totn:=ptr^.ncounthandle;
  totid:=totn+ptr^.idcountsize;
  ptr:=GetResInfoPtr(ptr^.subptr);
  ExtEnumResourceTypes:=true;
  i:=0;
  while i<totn do //named entries
  begin
    pc:=GetPChar(ptr[i].nameid);
    if not EnumFunc(ModuleHandle,pc,lParam) then exit;
    inc(i);
  end;
  while i<totid do
  begin
    if not EnumFunc(ModuleHandle,PChar(ptr[i].nameid),lParam) then exit;
    inc(i);
  end;
end;

function ExtEnumResourceNames(ModuleHandle : TFPResourceHMODULE; ResourceType : PChar; EnumFunc : EnumResNameProc; lParam : PtrInt) : LongBool;
var ptr : PResInfoNode;
    totn, totid, i : longword;
    pc : pchar;
begin
  ExtEnumResourceNames:=False;
  if ResHeader=nil then exit;
  ptr:=GetResInfoPtr(sizeof(TExtHeader));

  ptr:=BinSearchRes(ptr,ResourceType);
  if ptr=nil then exit;

  totn:=ptr^.ncounthandle;
  totid:=totn+ptr^.idcountsize;
  ptr:=GetResInfoPtr(ptr^.subptr);
  ExtEnumResourceNames:=true;
  i:=0;
  while i<totn do //named entries
  begin
    pc:=GetPChar(ptr[i].nameid);
    if not EnumFunc(ModuleHandle,ResourceType,pc,lParam) then exit;
    inc(i);
  end;
  while i<totid do
  begin
    if not EnumFunc(ModuleHandle,ResourceType,PChar(ptr[i].nameid),lParam) then exit;
    inc(i);
  end;
end;

function ExtEnumResourceLanguages(ModuleHandle : TFPResourceHMODULE; ResourceType, ResourceName : PChar; EnumFunc : EnumResLangProc; lParam : PtrInt) : LongBool;
var ptr : PResInfoNode;
    tot, i : integer;
begin
  ExtEnumResourceLanguages:=False;
  ptr:=InternalFindResource(ResourceName,ResourceType);
  if ptr=nil then exit;

  tot:=ptr^.idcountsize;
  ptr:=GetResInfoPtr(ptr^.subptr);
  ExtEnumResourceLanguages:=true;
  i:=0;
  while i<tot do
  begin
    if not EnumFunc(ModuleHandle,ResourceType,ResourceName,PtrUInt(ptr[i].nameid),lParam) then exit;
    inc(i);
  end;
end;

Function ExtFindResource(ModuleHandle: TFPResourceHMODULE; ResourceName, ResourceType: PChar)
: TFPResourceHandle;
var ptr : PResInfoNode;
begin
  ExtFindResource:=0;
  ptr:=InternalFindResource(ResourceName,ResourceType);
  if ptr=nil then exit;

  //first language id
  ptr:=GetResInfoPtr(ptr^.subptr);
  if ptr^.ncounthandle=0 then
  begin
    {$IFDEF EXTRES_MMAP}
    reshandles[usedhandles]:=ptr;
    {$ENDIF}
    {$IFDEF EXTRES_GENERIC}
    reshandles[usedhandles].info:=ptr;
    {$ENDIF}
    inc(usedhandles);
    ptr^.ncounthandle:=usedhandles;
  end;
  ExtFindResource:=ptr^.ncounthandle;
end;

Function ExtFindResourceEx(ModuleHandle: TFPResourceHMODULE; ResourceType,
  ResourceName: PChar; Language : word): TFPResourceHandle;
const LANG_NEUTRAL = 0;
      LANG_ENGLISH = 9;
var nameptr,ptr : PResInfoNode;
begin
  ExtFindResourceEx:=0;
  nameptr:=InternalFindResource(ResourceName,ResourceType);
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
    ptr:=GetResInfoPtr(nameptr^.subptr);

  if ptr^.ncounthandle=0 then
  begin
    {$IFDEF EXTRES_MMAP}
    reshandles[usedhandles]:=ptr;
    {$ENDIF}
    {$IFDEF EXTRES_GENERIC}
    reshandles[usedhandles].info:=ptr;
    {$ENDIF}
    inc(usedhandles);
    ptr^.ncounthandle:=usedhandles;
  end;
  ExtFindResourceEx:=ptr^.ncounthandle;
end;

{$IFDEF EXTRES_MMAP}
Function ExtLoadResource(ModuleHandle: TFPResourceHMODULE; ResHandle: TFPResourceHandle): TFPResourceHGLOBAL;
begin
  ExtLoadResource:=0;
  if ResHeader=nil then exit;
  if (ResHandle<=0) or (ResHandle>usedhandles) then exit;
  ExtLoadResource:=TFPResourceHGLOBAL(GetPtr(PResInfoNode(reshandles[ResHandle-1])^.subptr));
end;

Function ExtFreeResource(ResData: TFPResourceHGLOBAL): LongBool;
begin
  ExtFreeResource:=(ResHeader<>nil);
end;

Function ExtSizeofResource(ModuleHandle: TFPResourceHMODULE; ResHandle: TFPResourceHandle): LongWord;
begin
  ExtSizeofResource:=0;
  if ResHeader=nil then exit;
  if (ResHandle<=0) or (ResHandle>usedhandles) then exit;
  ExtSizeofResource:=PResInfoNode(reshandles[ResHandle-1])^.idcountsize;
end;
{$ENDIF}

{$IFDEF EXTRES_GENERIC}
(*
Resource data memory layout:
-2*sizeof(pointer)  Reference count
  -sizeof(pointer)  Pointer to resource info
                 0  Resource data
*)
Function ExtLoadResource(ModuleHandle: TFPResourceHMODULE; ResHandle: TFPResourceHandle): TFPResourceHGLOBAL;
var ptr : PPtrUInt;
    tmp : longword;
begin
  ExtLoadResource:=0;
  if ResHeader=nil then exit;
  if (ResHandle<=0) or (ResHandle>usedhandles) then exit;

  if reshandles[ResHandle-1].ptr=nil then
  begin
    {$I-}
    Seek(fd,reshandles[ResHandle-1].info^.subptr);
    {$I+}
    if IOResult<>0 then exit;
    ptr:=GetMem(reshandles[ResHandle-1].info^.idcountsize+2*sizeof(PtrUint));
    if ptr=nil then exit;
    ptr^:=1; //refcount
    inc(ptr);
    ptr^:=PtrUInt(reshandles[ResHandle-1].info); //ptr to resource info
    inc(ptr);
    {$I-}
    BlockRead(fd,ptr^,reshandles[ResHandle-1].info^.idcountsize,tmp);
    {$I+}
    if (IOResult<>0) or (tmp<>reshandles[ResHandle-1].info^.idcountsize) then
    begin
      FreeMem(ptr);
      exit;
    end;
    reshandles[ResHandle-1].ptr:=ptr;
  end
  else
  begin
    ptr:=reshandles[ResHandle-1].ptr;
    dec(ptr,2);
    inc(ptr^,1); //increase reference count
  end;
  ExtLoadResource:=TFPResourceHGLOBAL(reshandles[ResHandle-1].ptr);
end;

Function ExtFreeResource(ResData: TFPResourceHGLOBAL): LongBool;
var ptrinfo : PResInfoNode;
    ptr : PPtrUInt;
begin
  ExtFreeResource:=(ResHeader<>nil);
  if not ExtFreeResource then exit;
  ptr:=PPtrUInt(ResData);
  dec(ptr,2);
  dec(ptr^); //decrease reference count
  if ptr^=0 then
  begin
    inc(ptr);
    ptrinfo:=PResInfoNode(ptr^);
    dec(ptr);
    FreeMem(ptr);
    reshandles[ptrinfo^.ncounthandle-1].ptr:=nil;
  end;
  ExtFreeResource:=true;
end;

Function ExtSizeofResource(ModuleHandle: TFPResourceHMODULE; ResHandle: TFPResourceHandle): LongWord;
var ptrinfo : PResInfoNode;
begin
  ExtSizeofResource:=0;
  if ResHeader=nil then exit;
  if (ResHandle<=0) or (ResHandle>usedhandles) then exit;
  ptrinfo:=PResInfoNode(reshandles[ResHandle-1].info);
  ExtSizeofResource:=ptrinfo^.idcountsize;
end;
{$ENDIF}

Function ExtLockResource(ResData: TFPResourceHGLOBAL): Pointer;
begin
  ExtLockResource:=Nil;
  if ResHeader=nil then exit;
  ExtLockResource:=Pointer(ResData);
end;

Function ExtUnlockResource(ResData: TFPResourceHGLOBAL): LongBool;
begin
  ExtUnlockResource:=(ResHeader<>nil);
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

