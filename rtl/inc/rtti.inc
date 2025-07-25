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

const
  RTTISpecialSize = 49;
  RTTIManagedSizes: array[TTypeKind] of uint8 = { 0 — unmanaged, RTTISpecialSize — special (array/record/object), otherwise manBuiltin of that size. }
  (
    {tkUnknown} 0, {tkInteger} 0, {tkChar} 0, {tkEnumeration} 0, {tkFloat} 0,
    {tkSet} 0, {tkMethod} 0, {tkSString} 0, {tkLString} 0, {tkAString} sizeof(pointer),
    {tkWString} sizeof(pointer), {tkVariant} {$ifdef FPC_HAS_FEATURE_VARIANTS} sizeof(TVarData) {$else} 0 {$endif}, {tkArray} RTTISpecialSize, {tkRecord} RTTISpecialSize, {tkInterface} sizeof(pointer),
    {tkClass} 0, {tkObject} RTTISpecialSize, {tkWChar} 0, {tkBool} 0, {tkInt64} 0, {tkQWord} 0,
    {tkDynArray} sizeof(pointer), {tkInterfaceRaw} 0, {tkProcVar} 0, {tkUString} sizeof(pointer), {tkUChar} 0,
    {tkHelper} 0, {tkFile} 0, {tkClassRef} 0, {tkPointer} 0
  );

{ onlyCustomOps = false: returns true if the type requires any management regarding op.
  onlyCustomOps = true: returns true if the type has custom op, used in dynarr.inc to avoid initialization of zeroed memory. }
function RTTIManagementAndSize(typeInfo: Pointer; op: TRTTIRecOpType; out size: SizeInt; onlyCustomOps: boolean): boolean;
var
  ri: PRecordInfoInit;
  elem: PRecordElement;
  elemCount,sample,_size: SizeInt;
begin
  sample:=RTTIManagedSizes[PTypeKind(typeinfo)^];
  size:=sample;
  if sample<>RTTISpecialSize then
    result:=not onlyCustomOps and (sample<>0)
  else if PTypeKind(typeinfo)^=tkArray then
    begin
      typeInfo:=aligntoqword(typeInfo+2+PByte(typeInfo)[1]);
      size:=PArrayInfo(typeInfo)^.Size;
      result:=RTTIManagementAndSize(PArrayInfo(typeInfo)^.ElInfo^, op, _size, onlyCustomOps);
    end
  else {tkObject, tkRecord}
    begin
      ri:=RTTIRecordInfoInit(typeInfo);
      size:=ri^.Size;
      result:=Assigned(ri^.RecordOp) and Assigned(ri^.RecordOp^.Ops[op]);
      if result then
        exit;
      elem:=AlignTypeData(Pointer(@ri^.Count)+SizeOf(ri^.Count));
      for elemCount:=ri^.Count downto 1 do
        begin
          sample:=RTTIManagedSizes[PTypeKind(elem^.TypeInfo^)^]; { Avoid recursive call for simple fields. }
          result:=not onlyCustomOps and (sample<>0); { And generally speculate simple field. }
          if sample=RTTISpecialSize then
            result:=RTTIManagementAndSize(elem^.TypeInfo^, op, _size, onlyCustomOps);
          if result then
            exit;
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
      with PArrayInfo(aligntoqword(typeInfo+2+PByte(typeInfo)[1]))^ do
        int_InitializeArray(data,ElInfo^,ElCount);
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
      with PArrayInfo(aligntoqword(typeInfo+2+PByte(typeInfo)[1]))^ do
        int_FinalizeArray(data,ElInfo^,ElCount);
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
      with PArrayInfo(aligntoqword(typeInfo+2+PByte(typeInfo)[1]))^ do
        int_AddRefArray(data,ElInfo^,ElCount);
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


Function fpc_Copy (Src, Dest, TypeInfo : Pointer) : SizeInt;[Public,alias : 'FPC_COPY']; compilerproc;
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
      fpc_AnsiStr_Assign(PPointer(Dest)^,PPointer(Src)^);
{$endif FPC_HAS_FEATURE_ANSISTRINGS}
{$ifdef FPC_HAS_FEATURE_WIDESTRINGS}
  {$ifndef FPC_WIDESTRING_EQUAL_UNICODESTRING}
    tkWstring:
      fpc_WideStr_Assign(PPointer(Dest)^,PPointer(Src)^);
  {$endif FPC_WIDESTRING_EQUAL_UNICODESTRING}
    tkUstring:
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
            int_Copy(Src+Offset,Dest+Offset,Info);
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
                expectedoffset:=Offset+int_Copy(Src+Offset,Dest+Offset,PRecordElement(Temp)^.TypeInfo^);
                Inc(PRecordElement(Temp));
              end;
            { elements remaining? }
            if result>expectedoffset then
              move((Src+expectedoffset)^,(Dest+expectedoffset)^,Result-expectedoffset);
          end;
      end;
{$ifdef FPC_HAS_FEATURE_DYNARRAYS}
    tkDynArray:
      fpc_dynarray_assign(PPointer(Dest)^,PPointer(Src)^,typeinfo);
{$endif FPC_HAS_FEATURE_DYNARRAYS}
{$ifdef FPC_HAS_FEATURE_CLASSES}
    tkInterface:
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
procedure fpc_Copy_proc (Src, Dest, TypeInfo : Pointer);compilerproc; inline;
begin
  int_copy(src,dest,typeinfo);
end;


{$ifdef FPC_MANAGED_MOVE}
function fpc_Copy_with_move_semantics (Src, Dest, TypeInfo : Pointer) : SizeInt;[Public,alias : 'FPC_COPY_WITH_MOVE_SEMANTICS']; compilerproc;
var
  tki : pointer;
begin
  tki:=aligntoqword(typeInfo+2+PByte(typeInfo)[1]);
  if PTypeKind(TypeInfo)^=tkArray then { Only tkArray, tkObject, tkRecord are possible, search for 'fpc_copy_proc' in compiler/nld.pas. }
    result:=PArrayInfo(tki)^.Size
  else
    result:=PRecordInfoInit(tki)^.Size;
  int_finalize(Dest,TypeInfo);
  move(src^,dest^,result);
  int_initialize(Src,TypeInfo);
end;

procedure fpc_Copy_with_move_semantics_proc (Src, Dest, TypeInfo : Pointer);compilerproc; inline;
begin
  int_Copy_with_move_semantics(src,dest,typeinfo);
end;
{$endif FPC_MANAGED_MOVE}


procedure fpc_initialize_array(data,typeinfo : pointer;count : SizeInt); [public,alias:'FPC_INITIALIZE_ARRAY']; compilerproc;
  var
    sample,size,i : SizeInt;
  begin
    sample:=RTTIManagedSizes[PTypeKind(typeinfo)^];
    if sample<>RTTISpecialSize then
      FillChar(data^,sample*count,0)
    else if RTTIManagementAndSize(typeinfo, rotInitialize, size, false) then
      for i:=0 to count-1 do
        int_initialize(data+size*i,typeinfo);
  end;


procedure fpc_finalize_array(data,typeinfo : pointer;count : SizeInt); [Public,Alias:'FPC_FINALIZE_ARRAY'];  compilerproc;
  var
    size,i : SizeInt;
  begin
    if RTTIManagementAndSize(typeinfo, rotFinalize, size, false) then
      for i:=0 to count-1 do
        int_finalize(data+size*i,typeinfo);
  end;


procedure fpc_addref_array(data,typeinfo: pointer; count: SizeInt); [public,alias:'FPC_ADDREF_ARRAY']; compilerproc;
  var
    size,i : SizeInt;
  begin
    if RTTIManagementAndSize(typeinfo, rotAddRef, size, false) then
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

procedure AddRefArray(p, typeInfo: Pointer; count: SizeInt);
  external name 'FPC_ADDREF_ARRAY';

procedure CopyArray(dest, source, typeInfo: Pointer; count: SizeInt);
  var
    i, size: SizeInt;
  begin
    if RTTIManagementAndSize(typeinfo, rotCopy, size, false) then
      for i:=0 to count-1 do
        int_Copy(source+size*i, dest+size*i, typeInfo);
  end;

