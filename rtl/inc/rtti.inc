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

{ the tk* constants are now declared in system.inc }

type
  TRTTIProc=procedure(Data,TypeInfo:Pointer);

  PRecordElement=^TRecordElement;
  TRecordElement=packed record
    TypeInfo: Pointer;
    Offset: Longint;
  end;

  PRecordInfo=^TRecordInfo;
  TRecordInfo=packed record
    Size: Longint;
    Count: Longint;
    { Elements: array[count] of TRecordElement }
  end;

  PArrayInfo=^TArrayInfo;
  TArrayInfo=packed record
    ElSize: SizeInt;
    ElCount: SizeInt;
    ElInfo: Pointer;
  end;


function RTTIArraySize(typeInfo: Pointer): SizeInt;
begin
  typeInfo:=aligntoptr(typeInfo+2+PByte(typeInfo)[1]);
  result:=PArrayInfo(typeInfo)^.ElSize * PArrayInfo(typeInfo)^.ElCount;
end;

function RTTIRecordSize(typeInfo: Pointer): SizeInt;
begin
  typeInfo:=aligntoptr(typeInfo+2+PByte(typeInfo)[1]);
  result:=PRecordInfo(typeInfo)^.Size;
end;

function RTTISize(typeInfo: Pointer): SizeInt;
begin
  case PByte(typeinfo)^ of
    tkAString,tkWString,tkUString,
    tkInterface,tkDynarray:
      result:=sizeof(Pointer);
{$ifdef FPC_HAS_FEATURE_VARIANTS}
    tkVariant:
      result:=sizeof(TVarData);
{$endif FPC_HAS_FEATURE_VARIANTS}
    tkArray:
      result:=RTTIArraySize(typeinfo);
    tkObject,tkRecord:
      result:=RTTIRecordSize(typeinfo);
  else
    result:=-1;
  end;
end;

{ if you modify this procedure, fpc_copy must be probably modified as well }
procedure RecordRTTI(Data,TypeInfo:Pointer;rttiproc:TRTTIProc);
var
  count,
  i : longint;
begin
  typeInfo:=aligntoptr(typeInfo+2+PByte(typeInfo)[1]);
  Count:=PRecordInfo(typeInfo)^.Count;
  Inc(PRecordInfo(typeInfo));
  { Process elements }
  for i:=1 to count Do
    begin
      rttiproc (Data+PRecordElement(typeInfo)^.Offset,PRecordElement(typeInfo)^.TypeInfo);
      Inc(PRecordElement(typeInfo));
    end;
end;


{ if you modify this procedure, fpc_copy must be probably modified as well }
procedure ArrayRTTI(Data,TypeInfo:Pointer;rttiproc:TRTTIProc);
var
  i : SizeInt;
begin
  typeInfo:=aligntoptr(typeInfo+2+PByte(typeInfo)[1]);
  { Process elements }
  for I:=0 to PArrayInfo(typeInfo)^.ElCount-1 do
    rttiproc(Data+(I*PArrayInfo(typeInfo)^.ElSize),PArrayInfo(typeInfo)^.ElInfo);
end;


Procedure fpc_Initialize (Data,TypeInfo : pointer);[Public,Alias : 'FPC_INITIALIZE'];  compilerproc;
begin
  case PByte(TypeInfo)^ of
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
      PPchar(Data)^:=Nil;
    tkArray:
      arrayrtti(data,typeinfo,@int_initialize);
{$ifdef FPC_HAS_FEATURE_OBJECTS}
    tkObject,
{$endif FPC_HAS_FEATURE_OBJECTS}
    tkRecord:
      recordrtti(data,typeinfo,@int_initialize);
{$ifdef FPC_HAS_FEATURE_VARIANTS}
    tkVariant:
      variant_init(PVarData(Data)^);
{$endif FPC_HAS_FEATURE_VARIANTS}
  end;
end;


Procedure fpc_finalize (Data,TypeInfo: Pointer);[Public,Alias : 'FPC_FINALIZE'];  compilerproc;
begin
  case PByte(TypeInfo)^ of
{$ifdef FPC_HAS_FEATURE_ANSISTRINGS}
    tkAstring :
      begin
        fpc_AnsiStr_Decr_Ref(PPointer(Data)^);
        PPointer(Data)^:=nil;
      end;
{$endif FPC_HAS_FEATURE_ANSISTRINGS}
{$ifdef FPC_HAS_FEATURE_WIDESTRINGS}
    tkUstring :
      begin
        fpc_UnicodeStr_Decr_Ref(PPointer(Data)^);
        PPointer(Data)^:=nil;
      end;
  {$ifndef FPC_WIDESTRING_EQUAL_UNICODESTRING}
    tkWstring :
      begin
        fpc_WideStr_Decr_Ref(PPointer(Data)^);
        PPointer(Data)^:=nil;
      end;
  {$endif FPC_WIDESTRING_EQUAL_UNICODESTRING}
{$endif FPC_HAS_FEATURE_WIDESTRINGS}
    tkArray :
      arrayrtti(data,typeinfo,@int_finalize);
{$ifdef FPC_HAS_FEATURE_OBJECTS}
    tkObject,
{$endif FPC_HAS_FEATURE_OBJECTS}
    tkRecord:
      recordrtti(data,typeinfo,@int_finalize);
    tkInterface:
      begin
        Intf_Decr_Ref(PPointer(Data)^);
        PPointer(Data)^:=nil;
      end;
{$ifdef FPC_HAS_FEATURE_DYNARRAYS}
    tkDynArray:
      begin
        fpc_dynarray_decr_ref(PPointer(Data)^,TypeInfo);
        PPointer(Data)^:=nil;
      end;
{$endif FPC_HAS_FEATURE_DYNARRAYS}
{$ifdef FPC_HAS_FEATURE_VARIANTS}
    tkVariant:
      variant_clear(PVarData(Data)^);
{$endif FPC_HAS_FEATURE_VARIANTS}
  end;
end;


Procedure fpc_Addref (Data,TypeInfo : Pointer); [Public,alias : 'FPC_ADDREF'];  compilerproc;
begin
  case PByte(TypeInfo)^ of
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
      recordrtti(data,typeinfo,@int_addref);
{$ifdef FPC_HAS_FEATURE_DYNARRAYS}
    tkDynArray:
      fpc_dynarray_incr_ref(PPointer(Data)^);
{$endif FPC_HAS_FEATURE_DYNARRAYS}
    tkInterface:
      Intf_Incr_Ref(PPointer(Data)^);
{$ifdef FPC_HAS_FEATURE_VARIANTS}
    tkVariant:
      variant_addref(pvardata(Data)^);
{$endif FPC_HAS_FEATURE_DYNARRAYS}
  end;
end;


{ alias for internal use }
{ we use another name else the compiler gets puzzled because of the wrong forward def }
procedure fpc_systemDecRef (Data, TypeInfo : Pointer);[external name 'FPC_DECREF'];

Procedure fpc_DecRef (Data, TypeInfo : Pointer);[Public,alias : 'FPC_DECREF'];  compilerproc;
begin
  case PByte(TypeInfo)^ of
    { see AddRef for comment about below construct (JM) }
{$ifdef FPC_HAS_FEATURE_ANSISTRINGS}
    tkAstring:
      fpc_AnsiStr_Decr_Ref(PPointer(Data)^);
{$endif FPC_HAS_FEATURE_ANSISTRINGS}
{$ifdef FPC_HAS_FEATURE_WIDESTRINGS}
  {$ifndef FPC_WIDESTRING_EQUAL_UNICODESTRING}
    tkWstring:
      fpc_WideStr_Decr_Ref(PPointer(Data)^);
  {$endif FPC_WIDESTRING_EQUAL_UNICODESTRING}
    tkUString:
      fpc_UnicodeStr_Decr_Ref(PPointer(Data)^);
{$endif FPC_HAS_FEATURE_WIDESTRINGS}
    tkArray:
      arrayrtti(data,typeinfo,@fpc_systemDecRef);
{$ifdef FPC_HAS_FEATURE_OBJECTS}
    tkobject,
{$endif FPC_HAS_FEATURE_OBJECTS}
    tkrecord:
      recordrtti(data,typeinfo,@fpc_systemDecRef);
{$ifdef FPC_HAS_FEATURE_DYNARRAYS}
    tkDynArray:
      fpc_dynarray_decr_ref(PPointer(Data)^,TypeInfo);
{$endif FPC_HAS_FEATURE_DYNARRAYS}
    tkInterface:
      Intf_Decr_Ref(PPointer(Data)^);
{$ifdef FPC_HAS_FEATURE_VARIANTS}
    tkVariant:
      variant_clear(pvardata(data)^);
{$endif FPC_HAS_FEATURE_VARIANTS}
  end;
end;

{ define alias for internal use in the system unit }
Function fpc_Copy_internal (Src, Dest, TypeInfo : Pointer) : SizeInt;[external name 'FPC_COPY'];

Function fpc_Copy (Src, Dest, TypeInfo : Pointer) : SizeInt;[Public,alias : 'FPC_COPY']; compilerproc;
var
  ArrayInfo: PArrayInfo;
  Temp : pbyte;
  copiedsize,
  expectedoffset,
  count,
  offset,
  i : SizeInt;
  info : pointer;
begin
  result:=sizeof(pointer);
  case PByte(TypeInfo)^ of
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
        ArrayInfo:=aligntoptr(typeInfo+2+PByte(typeInfo)[1]);
        { Process elements }
        for I:=0 to ArrayInfo^.ElCount-1 do
          fpc_Copy_internal(Src+(I*ArrayInfo^.ElSize),Dest+(I*ArrayInfo^.ElSize),ArrayInfo^.ElInfo);
        Result:=ArrayInfo^.ElSize*ArrayInfo^.ElCount;
      end;
{$ifdef FPC_HAS_FEATURE_OBJECTS}
    tkobject,
{$endif FPC_HAS_FEATURE_OBJECTS}
    tkrecord:
      begin
        Temp:=aligntoptr(typeInfo+2+PByte(typeInfo)[1]);

        Result:=PRecordInfo(Temp)^.Size;
        Count:=PRecordInfo(Temp)^.Count;
        Inc(PRecordInfo(Temp));
        expectedoffset:=0;
        { Process elements with rtti }
        for i:=1 to count Do
          begin
            Info:=PRecordElement(Temp)^.TypeInfo;
            Offset:=PRecordElement(Temp)^.Offset;
            Inc(PRecordElement(Temp));
            if Offset>expectedoffset then
              move((Src+expectedoffset)^,(Dest+expectedoffset)^,Offset-expectedoffset);
            copiedsize:=fpc_Copy_internal(Src+Offset,Dest+Offset,Info);
            expectedoffset:=Offset+copiedsize;
          end;
        { elements remaining? }
        if result>expectedoffset then
          move((Src+expectedoffset)^,(Dest+expectedoffset)^,Result-expectedoffset);
      end;
{$ifdef FPC_HAS_FEATURE_DYNARRAYS}
    tkDynArray:
      fpc_dynarray_assign(PPointer(Dest)^,PPointer(Src)^,typeinfo);
{$endif FPC_HAS_FEATURE_DYNARRAYS}
    tkInterface:
      fpc_intf_assign(PPointer(Dest)^,PPointer(Src)^);
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


procedure fpc_initialize_array(data,typeinfo : pointer;count : SizeInt); [public,alias:'FPC_INITIALIZE_ARRAY'] compilerproc;
  var
     i, size : SizeInt;
  begin
     size:=RTTISize(typeinfo);
     if size>0 then
       for i:=0 to count-1 do
         int_initialize(data+size*i,typeinfo);
  end;


procedure fpc_finalize_array(data,typeinfo : pointer;count : SizeInt); [Public,Alias:'FPC_FINALIZE_ARRAY'];  compilerproc;
  var
     i, size: SizeInt;
  begin
     size:=RTTISize(typeinfo);
     if size>0 then
       for i:=0 to count-1 do
         int_finalize(data+size*i,typeinfo);
  end;

procedure fpc_addref_array(data,typeinfo: pointer; count: SizeInt); [public,alias:'FPC_ADDREF_ARRAY']; compilerproc;
  var
    i, size: SizeInt;
  begin
    size:=RTTISize(typeinfo);
    if size>0 then
      for i:=0 to count-1 do
        int_addref(data+size*i,typeinfo);
  end;

procedure fpc_decref_array(data,typeinfo: pointer; count: SizeInt); [public,alias:'FPC_DECREF_ARRAY']; compilerproc;
  var
    i, size: SizeInt;
  begin
    size:=RTTISize(typeinfo);
    if size>0 then
      for i:=0 to count-1 do
        int_decref(data+size*i,typeinfo);
  end;

