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

function RTTIArraySize(typeInfo: Pointer): SizeInt;
begin
  typeInfo:=aligntoqword(typeInfo+2+PByte(typeInfo)[1]);
  result:=PArrayInfo(typeInfo)^.Size;
end;

function RTTIRecordSize(typeInfo: Pointer): SizeInt;
begin
  typeInfo:=aligntoqword(typeInfo+2+PByte(typeInfo)[1]);
  { for size field init table is compatible with rtti table }
  result:=PRecordInfoFull(typeInfo)^.Size;
end;

function RTTIRecordOp(typeInfo: Pointer; var initrtti: Pointer): PRecordInfoInit; inline;
begin
  { find init table and management operators }
  typeInfo:=aligntoqword(typeInfo+2+PByte(typeInfo)[1]);
  result:=typeInfo;

  { check terminator, maybe we are already in init table }
  if Assigned(result^.Terminator) then
  begin
    { point to more optimal initrtti }
    initrtti:=PRecordInfoFull(result)^.InitTable;
    { and point to management operators in our init table }
    result:=aligntoqword(initrtti+2+PByte(initrtti)[1]);
  end
end;

{ result = manBuiltin means e.g. that initialization is simply zeroing and can be omitted if the memory is already zeroed, as in dynarr.inc. }
function RTTIManagementAndSize(typeInfo: Pointer; op: TRTTIRecOpType; out size: SizeInt): TRTTIManagement;
var
  ri: PRecordInfoInit;
  elem, eElem: PRecordElement;
  newMan: TRTTIManagement;
  _initrtti: pointer;
  _size: SizeInt;
begin
  case PTypeKind(typeinfo)^ of
    tkAString,tkWString,tkUString,
    tkInterface,tkDynarray:
      begin
        size:=sizeof(Pointer);
        result:=manBuiltin;
      end;
{$ifdef FPC_HAS_FEATURE_VARIANTS}
    tkVariant:
      begin
        size:=sizeof(TVarData);
        result:=manBuiltin;
      end;
{$endif FPC_HAS_FEATURE_VARIANTS}
    tkArray:
      begin
        typeInfo:=aligntoqword(typeInfo+2+PByte(typeInfo)[1]);
        size:=PArrayInfo(typeInfo)^.Size;
        result:=RTTIManagementAndSize(PArrayInfo(typeInfo)^.ElInfo^, op, _size);
      end;
    tkObject,
    tkRecord:
      begin
        ri:=RTTIRecordOp(typeInfo, _initrtti);
        size:=ri^.Size;
        if Assigned(ri^.RecordOp) and Assigned(ri^.RecordOp^.Ops[op]) then
          result:=manCustom
        else
          begin
            result:=manNone;
            elem:=AlignTypeData(Pointer(@ri^.Count)+SizeOf(ri^.Count));
            eElem:=elem+ri^.Count;
            while elem<>eElem do
              begin
                newMan:=RTTIManagementAndSize(elem^.TypeInfo^, op, _size);
                if newMan<>manNone then
                  result:=newMan;
                if newMan=manCustom then
                  break;
                inc(elem);
              end;
          end;
      end;
  else
    result:=manNone; { Size undefined, ultimately can be always correct to support unmanaged scenarios. }
  end;
end;

{ if you modify this procedure, fpc_copy must be probably modified as well }
procedure RecordRTTI(Data,TypeInfo:Pointer;rttiproc:TRTTIProc);
var
  count,
  i : longint;
begin
  typeInfo:=aligntoqword(typeInfo+2+PByte(typeInfo)[1]);
  Count:=PRecordInfoInit(typeInfo)^.Count;
  { Get element info, hacky, but what else can we do? }
  typeInfo:=AlignTypeData(Pointer(@PRecordInfoInit(typeInfo)^.Count)+SizeOf(PRecordInfoInit(typeInfo)^.Count));
  { Process elements }
  for i:=1 to count Do
    begin
      rttiproc(Data+PRecordElement(typeInfo)^.Offset,PRecordElement(typeInfo)^.TypeInfo^);
      Inc(PRecordElement(typeInfo));
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
      { if possible try to use more optimal initrtti }
      with RTTIRecordOp(typeinfo, typeinfo)^ do
      begin
        recordrtti(data,typeinfo,@int_initialize);
        if Assigned(recordop) and Assigned(recordop^.Initialize) then
          recordop^.Initialize(data);
      end;
{$ifdef FPC_HAS_FEATURE_VARIANTS}
    tkVariant:
      variant_init(PVarData(Data)^);
{$endif FPC_HAS_FEATURE_VARIANTS}
  end;
end;


Procedure fpc_finalize (Data,TypeInfo: Pointer);[Public,Alias : 'FPC_FINALIZE'];  compilerproc;
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
      { if possible try to use more optimal initrtti }
      with RTTIRecordOp(typeinfo, typeinfo)^ do
      begin
        if Assigned(recordop) and Assigned(recordop^.Finalize) then
          recordop^.Finalize(data);
        recordrtti(data,typeinfo,@int_finalize);
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
      { find init table }
      with RTTIRecordOp(typeinfo, typeinfo)^ do
      begin
        recordrtti(data,typeinfo,@int_addref);
        if Assigned(recordop) and Assigned(recordop^.AddRef) then
          recordop^.AddRef(Data);
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


{ define alias for internal use in the system unit }
Function fpc_Copy_internal (Src, Dest, TypeInfo : Pointer) : SizeInt;[external name 'FPC_COPY'];

Function fpc_Copy (Src, Dest, TypeInfo : Pointer) : SizeInt;[Public,alias : 'FPC_COPY']; compilerproc;
var
  Temp: pbyte;
  copiedsize,
  expectedoffset,
  EleCount,
  offset,
  i: SizeInt;
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
            fpc_Copy_internal(Src+Offset,Dest+Offset,Info);
            inc(Offset,copiedsize);
          end;
      end;
{$ifdef FPC_HAS_FEATURE_OBJECTS}
    tkobject,
{$endif FPC_HAS_FEATURE_OBJECTS}
    tkrecord:
      { find init table }
      with RTTIRecordOp(typeinfo, typeinfo)^ do
      begin
        Temp:=aligntoqword(typeInfo+2+PByte(typeInfo)[1]);
        if Assigned(recordop) and Assigned(recordop^.Copy) then
          begin
            recordop^.Copy(Src,Dest);
            Result:=PRecordInfoFull(Temp)^.Size;
          end
        else
          begin
            Result:=PRecordInfoInit(Temp)^.Size;
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
                expectedoffset:=Offset+fpc_Copy_internal(Src+Offset,Dest+Offset,PRecordElement(Temp)^.TypeInfo^);
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
  fpc_copy_internal(src,dest,typeinfo);
end;


procedure fpc_initialize_array(data,typeinfo : pointer;count : SizeInt); [public,alias:'FPC_INITIALIZE_ARRAY']; compilerproc;
  var
    i, size : SizeInt;
  begin
    if RTTIManagementAndSize(typeinfo, rotInitialize, size)<>manNone then
      for i:=0 to count-1 do
        int_initialize(data+size*i,typeinfo);
  end;


procedure fpc_finalize_array(data,typeinfo : pointer;count : SizeInt); [Public,Alias:'FPC_FINALIZE_ARRAY'];  compilerproc;
  var
    i, size : SizeInt;
  begin
    if RTTIManagementAndSize(typeinfo, rotFinalize, size)<>manNone then
      for i:=0 to count-1 do
        int_finalize(data+size*i,typeinfo);
  end;


procedure fpc_addref_array(data,typeinfo: pointer; count: SizeInt); [public,alias:'FPC_ADDREF_ARRAY']; compilerproc;
  var
    i, size : SizeInt;
  begin
    if RTTIManagementAndSize(typeinfo, rotAddRef, size)<>manNone then
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
    if RTTIManagementAndSize(typeinfo, rotCopy, size)<>manNone then
      for i:=0 to count-1 do
        fpc_Copy_internal(source+size*i, dest+size*i, typeInfo);
  end;

