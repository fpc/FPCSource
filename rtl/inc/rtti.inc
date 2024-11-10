{
    This file is part of the Free Pascal run time library.
    Copyright (c) 1999-2000 by Michael Van Canneyt
    member of the Free Pascal development team

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}

{ Run-Time type information routines }

function RTTIRecordInfoInit(typeInfo: Pointer): PRecordInfoInit; inline;
begin
  { find init table and management operators }
  result:=aligntoqword(typeInfo+2+PByte(typeInfo)[1]);

  { check terminator, maybe we are already in init table }
  if Assigned(result^.Terminator) then
  begin
    { point to more optimal initrtti }
    result:=PRecordInfoFull(result)^.InitTable;
    { and point to management operators in our init table }
    result:=aligntoqword(pointer(result)+2+PByte(result)[1]);
  end
end;

{ result = manBuiltin means e.g. that initialization is simply zeroing and can be omitted if the memory is already zeroed, as in dynarr.inc. }
function RTTIManagementAndSize(typeInfo: Pointer; op: TRTTIRecOpType; out size: SizeInt; maxInteresting: TRTTIManagement): TRTTIManagement;
const
  Special = 49;
  ManagedSizes: array[TTypeKind] of uint8 = { 0 — unmanaged, Special — special, otherwise manBuiltin of that size. }
  (
    {tkUnknown} 0, {tkInteger} 0, {tkChar} 0, {tkEnumeration} 0, {tkFloat} 0,
    {tkSet} 0, {tkMethod} 0, {tkSString} 0, {tkLString} 0, {tkAString} sizeof(pointer),
    {tkWString} sizeof(pointer), {tkVariant} {$ifdef FPC_HAS_FEATURE_VARIANTS} sizeof(TVarData) {$else} 0 {$endif}, {tkArray} Special, {tkRecord} Special, {tkInterface} sizeof(pointer),
    {tkClass} 0, {tkObject} Special, {tkWChar} 0, {tkBool} 0, {tkInt64} 0, {tkQWord} 0,
    {tkDynArray} sizeof(pointer), {tkInterfaceRaw} 0, {tkProcVar} 0, {tkUString} sizeof(pointer), {tkUChar} 0,
    {tkHelper} 0, {tkFile} 0, {tkClassRef} 0, {tkPointer} 0
  );
var
  ri: PRecordInfoInit;
  elem: PRecordElement;
  newMan: TRTTIManagement;
  elemCount,sample,_size: SizeInt;
begin
  sample:=ManagedSizes[PTypeKind(typeinfo)^];
  size:=sample;
  if sample<>Special then
    result:=TRTTIManagement(ord(sample<>0)) { manNone(0) if sample = 0, manBuiltin(1) otherwise. }
  else if PTypeKind(typeinfo)^=tkArray then
    begin
      typeInfo:=aligntoqword(typeInfo+2+PByte(typeInfo)[1]);
      size:=PArrayInfo(typeInfo)^.Size;
      result:=RTTIManagementAndSize(PArrayInfo(typeInfo)^.ElInfo^, op, _size, maxInteresting);
    end
  else {tkObject, tkRecord}
    begin
      ri:=RTTIRecordInfoInit(typeInfo);
      size:=ri^.Size;
      if Assigned(ri^.RecordOp) and Assigned(ri^.RecordOp^.Ops[op]) then
        exit(manCustom);
      result:=manNone;
      elem:=AlignTypeData(Pointer(@ri^.Count)+SizeOf(ri^.Count));
      for elemCount:=ri^.Count downto 1 do
        begin
          sample:=ManagedSizes[PTypeKind(elem^.TypeInfo^)^];
          if sample<>Special then
            newMan:=TRTTIManagement(ord(sample<>0)) { Avoid recursive call for simple fields. }
          else
            newMan:=RTTIManagementAndSize(elem^.TypeInfo^, op, _size, maxInteresting);
          if newMan>result then
            begin
              result:=newMan;
              if newMan>=maxInteresting then
                break;
            end;
          inc(elem);
        end;
    end;
end;

{ if you modify this procedure, fpc_copy must be probably modified as well }
procedure RecordRTTI(Data:Pointer;Ri:PRecordInfoInit;rttiproc:TRTTIProc);
var
  i : longint;
  Re : PRecordElement;
begin
  { Get element info, hacky, but what else can we do? }
  Re:=AlignTypeData(Pointer(@Ri^.Count)+SizeOf(Ri^.Count));
  { Process elements }
  for i:=Ri^.Count downto 1 Do
    begin
      rttiproc(Data+Re^.Offset,Re^.TypeInfo^);
      Inc(Re);
    end;
end;

function RTTIRecordMopInitTable(ti: Pointer): PRTTIRecordOpOffsetTable;
begin
  ti:=aligntoqword(ti+2+PByte(ti)[1]);
  Result:=PRecordInfoInit(ti)^.InitRecordOpTable;
end;

{ if you modify this procedure, fpc_copy must be probably modified as well }
procedure ArrayRTTI(Data,TypeInfo:Pointer;rttiproc:TRTTIProc);
var
  i,Count,ElSize: SizeInt;
  Info: Pointer;
begin
  typeInfo:=aligntoqword(typeInfo+2+PByte(typeInfo)[1]);
  Count:=PArrayInfo(typeInfo)^.ElCount;
  { no elements to process => exit }
  if Count = 0 then
    Exit;
  ElSize:=PArrayInfo(typeInfo)^.Size div Count;
  Info:=PArrayInfo(typeInfo)^.ElInfo^;
  { Process elements }
  for I:=0 to Count-1 do
    rttiproc(Data+(I*ElSize),Info);
end;

Procedure fpc_Initialize (Data,TypeInfo : pointer);[Public,Alias : 'FPC_INITIALIZE'];  compilerproc;
var
  ri: PRecordInfoInit;
begin
  case PTypeKind(TypeInfo)^ of
{$ifdef FPC_HAS_FEATURE_DYNARRAYS}
    tkDynArray,
{$endif FPC_HAS_FEATURE_DYNARRAYS}
{$ifdef FPC_HAS_FEATURE_ANSISTRINGS}
    tkAstring,
{$endif FPC_HAS_FEATURE_ANSISTRINGS}
{$ifdef FPC_HAS_FEATURE_WIDESTRINGS}
    tkWstring,tkUString,
{$endif FPC_HAS_FEATURE_WIDESTRINGS}
    tkInterface:
      PPAnsiChar(Data)^:=Nil;
    tkArray:
      arrayrtti(data,typeinfo,@int_initialize);
{$ifdef FPC_HAS_FEATURE_OBJECTS}
    tkObject,
{$endif FPC_HAS_FEATURE_OBJECTS}
    tkRecord:
      begin
        ri:=RTTIRecordInfoInit(typeinfo);
        recordrtti(data,ri,@int_initialize);
        if Assigned(ri^.recordop) and Assigned(ri^.recordop^.Initialize) then
          ri^.recordop^.Initialize(data);
      end;
{$ifdef FPC_HAS_FEATURE_VARIANTS}
    tkVariant:
      variant_init(PVarData(Data)^);
{$endif FPC_HAS_FEATURE_VARIANTS}
  end;
end;


Procedure fpc_finalize (Data,TypeInfo: Pointer);[Public,Alias : 'FPC_FINALIZE'];  compilerproc;
var
  ri: PRecordInfoInit;
begin
  case PTypeKind(TypeInfo)^ of
{$ifdef FPC_HAS_FEATURE_ANSISTRINGS}
    tkAstring :
      fpc_AnsiStr_Decr_Ref(PPointer(Data)^);
{$endif FPC_HAS_FEATURE_ANSISTRINGS}
{$ifdef FPC_HAS_FEATURE_WIDESTRINGS}
    tkUstring :
      fpc_UnicodeStr_Decr_Ref(PPointer(Data)^);
  {$ifndef FPC_WIDESTRING_EQUAL_UNICODESTRING}
    tkWstring :
      fpc_WideStr_Decr_Ref(PPointer(Data)^);
  {$endif FPC_WIDESTRING_EQUAL_UNICODESTRING}
{$endif FPC_HAS_FEATURE_WIDESTRINGS}
    tkArray :
      arrayrtti(data,typeinfo,@int_finalize);
{$ifdef FPC_HAS_FEATURE_OBJECTS}
    tkObject,
{$endif FPC_HAS_FEATURE_OBJECTS}
    tkRecord:
      begin
        ri:=RTTIRecordInfoInit(typeinfo);
        if Assigned(ri^.recordop) and Assigned(ri^.recordop^.Finalize) then
          ri^.recordop^.Finalize(data);
        recordrtti(data,ri,@int_finalize);
      end;
{$ifdef FPC_HAS_FEATURE_CLASSES}
    tkInterface:
      Intf_Decr_Ref(PPointer(Data)^);
{$endif FPC_HAS_FEATURE_CLASSES}
{$ifdef FPC_HAS_FEATURE_DYNARRAYS}
    tkDynArray:
      fpc_dynarray_clear(PPointer(Data)^,TypeInfo);
{$endif FPC_HAS_FEATURE_DYNARRAYS}
{$ifdef FPC_HAS_FEATURE_VARIANTS}
    tkVariant:
      variant_clear(PVarData(Data)^);
{$endif FPC_HAS_FEATURE_VARIANTS}
  end;
end;


Procedure fpc_Addref (Data,TypeInfo : Pointer); [Public,alias : 'FPC_ADDREF'];  compilerproc;
var
  ri: PRecordInfoInit;
begin
  case PTypeKind(TypeInfo)^ of
{$ifdef FPC_HAS_FEATURE_ANSISTRINGS}
    tkAstring :
      fpc_AnsiStr_Incr_Ref(PPointer(Data)^);
{$endif FPC_HAS_FEATURE_ANSISTRINGS}
{$ifdef FPC_HAS_FEATURE_WIDESTRINGS}
  {$ifndef FPC_WIDESTRING_EQUAL_UNICODESTRING}
    tkWstring :
      fpc_WideStr_Incr_Ref(PPointer(Data)^);
  {$endif FPC_WIDESTRING_EQUAL_UNICODESTRING}
    tkUstring :
      fpc_UnicodeStr_Incr_Ref(PPointer(Data)^);
{$endif FPC_HAS_FEATURE_WIDESTRINGS}
    tkArray :
      arrayrtti(data,typeinfo,@int_addref);
{$ifdef FPC_HAS_FEATURE_OBJECTS}
    tkobject,
{$endif FPC_HAS_FEATURE_OBJECTS}
    tkrecord :
      begin
        ri:=RTTIRecordInfoInit(typeinfo);
        recordrtti(data,ri,@int_addref);
        if Assigned(ri^.recordop) and Assigned(ri^.recordop^.AddRef) then
          ri^.recordop^.AddRef(Data);
      end;
{$ifdef FPC_HAS_FEATURE_DYNARRAYS}
    tkDynArray:
      fpc_dynarray_incr_ref(PPointer(Data)^);
{$endif FPC_HAS_FEATURE_DYNARRAYS}
{$ifdef FPC_HAS_FEATURE_CLASSES}
    tkInterface:
      Intf_Incr_Ref(PPointer(Data)^);
{$endif FPC_HAS_FEATURE_CLASSES}
{$ifdef FPC_HAS_FEATURE_VARIANTS}
    tkVariant:
      variant_addref(pvardata(Data)^);
{$endif FPC_HAS_FEATURE_DYNARRAYS}
  end;
end;

{$ifdef FPC_MANAGED_MOVE}
{ define alias for internal use in the system unit }
Function fpc_Copy_internal (Src, Dest, TypeInfo : Pointer; DoMove : Boolean) : SizeInt;[external name 'FPC_COPY'];

Function fpc_Copy (Src, Dest, TypeInfo : Pointer; DoMove : Boolean) : SizeInt;[Public,alias : 'FPC_COPY']; compilerproc;
{$else}
{ define alias for internal use in the system unit }
Function fpc_Copy_internal (Src, Dest, TypeInfo : Pointer) : SizeInt;[external name 'FPC_COPY'];

Function fpc_Copy (Src, Dest, TypeInfo : Pointer) : SizeInt;[Public,alias : 'FPC_COPY']; compilerproc;
{$endif FPC_MANAGED_MOVE}
var
  copiedsize,
  expectedoffset,
  EleCount,
  offset,
  i: SizeInt;
  Temp,
  info: pointer;
begin
  result:=sizeof(pointer);
  case PTypeKind(TypeInfo)^ of
{$ifdef FPC_HAS_FEATURE_ANSISTRINGS}
    tkAstring:
{$ifdef FPC_MANAGED_MOVE}
      if domove then
        begin
          fpc_AnsiStr_Decr_Ref(PPointer(Dest)^);
          PPointer(Dest)^:=PPointer(src)^;
          PPointer(Src)^:=nil;
        end
      else
{$endif FPC_MANAGED_MOVE}
      fpc_AnsiStr_Assign(PPointer(Dest)^,PPointer(Src)^);
{$endif FPC_HAS_FEATURE_ANSISTRINGS}
{$ifdef FPC_HAS_FEATURE_WIDESTRINGS}
  {$ifndef FPC_WIDESTRING_EQUAL_UNICODESTRING}
    tkWstring:
{$ifdef FPC_MANAGED_MOVE}
      if domove then
        begin
          fpc_WideStr_Decr_Ref(PPointer(Dest)^);
          PPointer(Dest)^:=PPointer(src)^;
          PPointer(Src)^:=nil;
        end
      else
{$endif FPC_MANAGED_MOVE}
      fpc_WideStr_Assign(PPointer(Dest)^,PPointer(Src)^);
  {$endif FPC_WIDESTRING_EQUAL_UNICODESTRING}
    tkUstring:
{$ifdef FPC_MANAGED_MOVE}
      if domove then
        begin
          fpc_UnicodeStr_Decr_Ref(PPointer(Dest)^);
          PPointer(Dest)^:=PPointer(src)^;
          PPointer(Src)^:=nil;
        end
      else
{$endif FPC_MANAGED_MOVE}
      fpc_UnicodeStr_Assign(PPointer(Dest)^,PPointer(Src)^);
{$endif FPC_HAS_FEATURE_WIDESTRINGS}
    tkArray:
      begin
        Temp:=aligntoqword(typeInfo+2+PByte(typeInfo)[1]);
        Result:=PArrayInfo(Temp)^.Size;
        EleCount:=PArrayInfo(Temp)^.ElCount;
        { no elements to process => exit }
        if EleCount = 0 then
          Exit;
        Info:=PArrayInfo(Temp)^.ElInfo^;
        copiedsize:=Result div EleCount;
        Offset:=0;
        { Process elements }
        for I:=1 to EleCount do
          begin
{$ifdef FPC_MANAGED_MOVE}
            fpc_Copy_internal(Src+Offset,Dest+Offset,Info,DoMove);
{$else}
            fpc_Copy_internal(Src+Offset,Dest+Offset,Info);
{$endif FPC_MANAGED_MOVE}
            inc(Offset,copiedsize);
          end;
      end;
{$ifdef FPC_HAS_FEATURE_OBJECTS}
    tkobject,
{$endif FPC_HAS_FEATURE_OBJECTS}
    tkrecord:
      begin
        Temp:=RTTIRecordInfoInit(typeinfo);
        Result:=PRecordInfoInit(Temp)^.Size;
{$ifdef FPC_MANAGED_MOVE}
        if domove then
          begin
            int_finalize(Dest,TypeInfo);
            move(src^,dest^,result);
            int_initialize(Src,TypeInfo);
          end
        else
{$endif FPC_MANAGED_MOVE}
        if Assigned(PRecordInfoInit(Temp)^.recordop) and Assigned(PRecordInfoInit(Temp)^.recordop^.Copy) then
          PRecordInfoInit(Temp)^.recordop^.Copy(Src,Dest)
        else
          begin
            EleCount:=PRecordInfoInit(Temp)^.Count;
            { Get element info, hacky, but what else can we do? }
            Temp:=AlignTypeData(Pointer(@PRecordInfoInit(Temp)^.Count)+SizeOf(PRecordInfoInit(Temp)^.Count));
            expectedoffset:=0;
            { Process elements with rtti }
            for i:=1 to EleCount Do
              begin
                Offset:=PRecordElement(Temp)^.Offset;
                if Offset>expectedoffset then
                  move((Src+expectedoffset)^,(Dest+expectedoffset)^,Offset-expectedoffset);
{$ifdef FPC_MANAGED_MOVE}
                expectedoffset:=Offset+fpc_Copy_internal(Src+Offset,Dest+Offset,PRecordElement(Temp)^.TypeInfo^,DoMove);
{$else}
                expectedoffset:=Offset+fpc_Copy_internal(Src+Offset,Dest+Offset,PRecordElement(Temp)^.TypeInfo^);
{$endif FPC_MANAGED_MOVE}
                Inc(PRecordElement(Temp));
              end;
            { elements remaining? }
            if result>expectedoffset then
              move((Src+expectedoffset)^,(Dest+expectedoffset)^,Result-expectedoffset);
          end;
      end;
{$ifdef FPC_HAS_FEATURE_DYNARRAYS}
    tkDynArray:
{$ifdef FPC_MANAGED_MOVE}
      if domove then
        begin
          fpc_dynarray_clear(PPointer(Dest)^,TypeInfo);
          PPointer(Dest)^:=PPointer(src)^;
          PPointer(Src)^:=nil;
        end
      else
{$endif FPC_MANAGED_MOVE}
      fpc_dynarray_assign(PPointer(Dest)^,PPointer(Src)^,typeinfo);
{$endif FPC_HAS_FEATURE_DYNARRAYS}
{$ifdef FPC_HAS_FEATURE_CLASSES}
    tkInterface:
{$ifdef FPC_MANAGED_MOVE}
      if domove then
        begin
          PPointer(Dest)^:=PPointer(src)^;
          PPointer(Src)^:=nil;
        end
      else
{$endif FPC_MANAGED_MOVE}
      fpc_intf_assign(PPointer(Dest)^,PPointer(Src)^);
{$endif FPC_HAS_FEATURE_CLASSES}
{$ifdef FPC_HAS_FEATURE_VARIANTS}
    tkVariant:
      begin
        VarCopyProc(pvardata(dest)^,pvardata(src)^);
        result:=sizeof(tvardata);
      end;
{$endif FPC_HAS_FEATURE_VARIANTS}
  end;
end;


{ For internal use by the compiler, because otherwise $x- can cause trouble. }
{ Generally disabling extended syntax checking for all compilerprocs may     }
{ have unintended side-effects                                               }
{$ifdef FPC_MANAGED_MOVE}
procedure fpc_Copy_proc (Src, Dest, TypeInfo : Pointer; DoMove : Boolean);compilerproc; inline;
begin
  fpc_copy_internal(src,dest,typeinfo,domove);
end;
{$else}
procedure fpc_Copy_proc (Src, Dest, TypeInfo : Pointer);compilerproc; inline;
begin
  fpc_copy_internal(src,dest,typeinfo);
end;
{$endif FPC_MANAGED_MOVE}


procedure fpc_initialize_array(data,typeinfo : pointer;count : SizeInt); [public,alias:'FPC_INITIALIZE_ARRAY']; compilerproc;
  var
    i, size : SizeInt;
  begin
    if RTTIManagementAndSize(typeinfo, rotInitialize, size, manBuiltin)<>manNone then
      for i:=0 to count-1 do
        int_initialize(data+size*i,typeinfo);
  end;


procedure fpc_finalize_array(data,typeinfo : pointer;count : SizeInt); [Public,Alias:'FPC_FINALIZE_ARRAY'];  compilerproc;
  var
    i, size : SizeInt;
  begin
    if RTTIManagementAndSize(typeinfo, rotFinalize, size, manBuiltin)<>manNone then
      for i:=0 to count-1 do
        int_finalize(data+size*i,typeinfo);
  end;


procedure fpc_addref_array(data,typeinfo: pointer; count: SizeInt); [public,alias:'FPC_ADDREF_ARRAY']; compilerproc;
  var
    i, size : SizeInt;
  begin
    if RTTIManagementAndSize(typeinfo, rotAddRef, size, manBuiltin)<>manNone then
      for i:=0 to count-1 do
        int_addref(data+size*i,typeinfo);
  end;

{ The following two procedures are now obsolete, needed only for bootstrapping }
procedure fpc_decref (Data, TypeInfo : Pointer);[Public,alias : 'FPC_DECREF'];  compilerproc;
  begin
    int_finalize(Data,TypeInfo);
  end;

procedure fpc_decref_array(data,typeinfo: pointer; count: SizeInt); [public,alias:'FPC_DECREF_ARRAY']; compilerproc;
  begin
    int_finalizeArray(data,typeinfo,count);
  end;

procedure InitializeArray(p, typeInfo: Pointer; count: SizeInt);
  external name 'FPC_INITIALIZE_ARRAY';

procedure FinalizeArray(p, typeInfo: Pointer; count: SizeInt);
  external name 'FPC_FINALIZE_ARRAY';

procedure CopyArray(dest, source, typeInfo: Pointer; count: SizeInt);
  var
    i, size: SizeInt;
  begin
    if RTTIManagementAndSize(typeinfo, rotCopy, size, manBuiltin)<>manNone then
      for i:=0 to count-1 do
{$ifdef FPC_MANAGED_MOVE}
        fpc_Copy_internal(source+size*i, dest+size*i, typeInfo, False);
{$else}
        fpc_Copy_internal(source+size*i, dest+size*i, typeInfo);
{$endif FPC_MANAGED_MOVE}
  end;

