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

{ The RTTI is implemented through a series of constants : }

Const
       tkUnknown       = 0;
       tkInteger       = 1;
       tkChar          = 2;
       tkEnumeration   = 3;
       tkFloat         = 4;
       tkSet           = 5;
       tkMethod        = 6;
       tkSString       = 7;
       tkString        = tkSString;
       tkLString       = 8;
       tkAString       = 9;
       tkWString       = 10;
       tkVariant       = 11;
       tkArray         = 12;
       tkRecord        = 13;
       tkInterface     = 14;
       tkClass         = 15;
       tkObject        = 16;
       tkWChar         = 17;
       tkBool          = 18;
       tkInt64         = 19;
       tkQWord         = 20;
       tkDynArray      = 21;
       tkInterfaceCorba = 22;
       tkProcVar       = 23;
       tkUString       = 24;


type
  TRTTIProc=procedure(Data,TypeInfo:Pointer);

{ if you modify this procedure, fpc_copy must be probably modified as well }
procedure RecordRTTI(Data,TypeInfo:Pointer;rttiproc:TRTTIProc);
{
  A record is designed as follows :
    1    : tkrecord
    2    : Length of name string (n);
    3    : name string;
    3+n  : record size;
    7+n  : number of elements (N)
    11+n : N times : Pointer to type info
                     Offset in record
}
var
  Temp : pbyte;
  namelen : byte;
  count,
  offset,
  i : longint;
  info : pointer;
begin
  Temp:=PByte(TypeInfo);
  inc(Temp);
  { Skip Name }
  namelen:=Temp^;
  inc(temp,namelen+1);
  temp:=aligntoptr(temp);
  { Skip size }
  inc(Temp,4);
  { Element count }
  Count:=PLongint(Temp)^;
  inc(Temp,sizeof(Count));
  { Process elements }
  for i:=1 to count Do
    begin
      Info:=PPointer(Temp)^;
      inc(Temp,sizeof(Info));
      Offset:=PLongint(Temp)^;
      inc(Temp,sizeof(Offset));
      rttiproc (Data+Offset,Info);
    end;
end;


{ if you modify this procedure, fpc_copy must be probably modified as well }
procedure ArrayRTTI(Data,TypeInfo:Pointer;rttiproc:TRTTIProc);
{
  An array is designed as follows :
   1    : tkArray;
   2    : length of name string (n);
   3    : NAme string
   3+n  : Element Size
   7+n  : Number of elements
   11+n : Pointer to type of elements
}
var
  Temp : pbyte;
  namelen : byte;
  count,
  size,
  i : SizeInt;
  info : pointer;
begin
  Temp:=PByte(TypeInfo);
  inc(Temp);
  { Skip Name }
  namelen:=Temp^;
  inc(temp,namelen+1);
  temp:=aligntoptr(temp);
  { Element size }
  size:=PSizeInt(Temp)^;
  inc(Temp,sizeof(Size));
  { Element count }
  Count:=PSizeInt(Temp)^;
  inc(Temp,sizeof(Count));
  Info:=PPointer(Temp)^;
  inc(Temp,sizeof(Info));
  { Process elements }
  for I:=0 to Count-1 do
    rttiproc(Data+(I*size),Info);
end;


Procedure fpc_Initialize (Data,TypeInfo : pointer);[Public,Alias : 'FPC_INITIALIZE'];  compilerproc;
begin
  case PByte(TypeInfo)^ of
    tkAstring,tkWstring,tkUString,tkInterface,tkDynArray:
      PPchar(Data)^:=Nil;
    tkArray:
      arrayrtti(data,typeinfo,@int_initialize);
    tkObject,
    tkRecord:
      recordrtti(data,typeinfo,@int_initialize);
    tkVariant:
      variant_init(PVarData(Data)^);
  end;
end;


Procedure fpc_finalize (Data,TypeInfo: Pointer);[Public,Alias : 'FPC_FINALIZE'];  compilerproc;
begin
  case PByte(TypeInfo)^ of
    tkAstring :
      begin
        fpc_AnsiStr_Decr_Ref(PPointer(Data)^);
        PPointer(Data)^:=nil;
      end;
{$ifndef VER2_2}
    tkUstring :
      begin
        fpc_UnicodeStr_Decr_Ref(PPointer(Data)^);
        PPointer(Data)^:=nil;
      end;
{$endif VER2_2}
{$ifdef WINDOWS}
    tkWstring :
      begin
        fpc_WideStr_Decr_Ref(PPointer(Data)^);
        PPointer(Data)^:=nil;
      end;
{$endif WINDOWS}
    tkArray :
      arrayrtti(data,typeinfo,@int_finalize);
    tkObject,
    tkRecord:
      recordrtti(data,typeinfo,@int_finalize);
    tkInterface:
      begin
        Intf_Decr_Ref(PPointer(Data)^);
        PPointer(Data)^:=nil;
      end;
    tkDynArray:
      fpc_dynarray_decr_ref(PPointer(Data)^,TypeInfo);
    tkVariant:
      variant_clear(PVarData(Data)^);
  end;
end;


Procedure fpc_Addref (Data,TypeInfo : Pointer); [Public,alias : 'FPC_ADDREF'];  compilerproc;
begin
  case PByte(TypeInfo)^ of
    tkAstring :
      fpc_AnsiStr_Incr_Ref(PPointer(Data)^);
{$ifdef WINDOWS}
    tkWstring :
      fpc_WideStr_Incr_Ref(PPointer(Data)^);
{$endif WINDOWS}
{$ifndef VER2_2}
    tkUstring :
      fpc_UnicodeStr_Incr_Ref(PPointer(Data)^);
{$endif VER2_2}
    tkArray :
      arrayrtti(data,typeinfo,@int_addref);
    tkobject,
    tkrecord :
      recordrtti(data,typeinfo,@int_addref);
    tkDynArray:
      fpc_dynarray_incr_ref(PPointer(Data)^);
    tkInterface:
      Intf_Incr_Ref(PPointer(Data)^);
    tkVariant:
      variant_addref(pvardata(Data)^);
  end;
end;


{ alias for internal use }
{ we use another name else the compiler gets puzzled because of the wrong forward def }
procedure fpc_systemDecRef (Data, TypeInfo : Pointer);[external name 'FPC_DECREF'];

Procedure fpc_DecRef (Data, TypeInfo : Pointer);[Public,alias : 'FPC_DECREF'];  compilerproc;
begin
  case PByte(TypeInfo)^ of
    { see AddRef for comment about below construct (JM) }
    tkAstring:
      fpc_AnsiStr_Decr_Ref(PPointer(Data)^);
{$ifdef WINDOWS}
    tkWstring:
      fpc_WideStr_Decr_Ref(PPointer(Data)^);
{$endif WINDOWS}
{$ifndef VER2_2}
    tkUString:
      fpc_UnicodeStr_Decr_Ref(PPointer(Data)^);
{$endif VER2_2}
    tkArray:
      arrayrtti(data,typeinfo,@fpc_systemDecRef);
    tkobject,
    tkrecord:
      recordrtti(data,typeinfo,@fpc_systemDecRef);
    tkDynArray:
      fpc_dynarray_decr_ref(PPointer(Data)^,TypeInfo);
    tkInterface:
      Intf_Decr_Ref(PPointer(Data)^);
    tkVariant:
      variant_clear(pvardata(data)^);
  end;
end;

{ define alias for internal use in the system unit }
Function fpc_Copy_internal (Src, Dest, TypeInfo : Pointer) : SizeInt;[external name 'FPC_COPY'];

Function fpc_Copy (Src, Dest, TypeInfo : Pointer) : SizeInt;[Public,alias : 'FPC_COPY']; compilerproc;
var
  Temp : pbyte;
  namelen : byte;
  copiedsize,
  expectedoffset,
  count,
  offset,
  size,
  i : SizeInt;
  info : pointer;
begin
  result:=sizeof(pointer);
  case PByte(TypeInfo)^ of
    tkAstring:
      begin
        fpc_AnsiStr_Incr_Ref(PPointer(Src)^);
        fpc_AnsiStr_Decr_Ref(PPointer(Dest)^);
        PPointer(Dest)^:=PPointer(Src)^;
      end;
{$ifdef WINDOWS}
    tkWstring:
      fpc_WideStr_Assign(PPointer(Dest)^,PPointer(Src)^);
{$endif WINDOWS}
{$ifndef VER2_2}
    tkUstring:
      fpc_UnicodeStr_Assign(PPointer(Dest)^,PPointer(Src)^);
{$endif VER2_2}
    tkArray:
      begin
        Temp:=PByte(TypeInfo);
        inc(Temp);
        { Skip Name }
        namelen:=Temp^;
        inc(temp,namelen+1);
        temp:=aligntoptr(temp);

        { Element size }
        size:=PSizeInt(Temp)^;
        inc(Temp,sizeof(Size));

        { Element count }
        Count:=PSizeInt(Temp)^;
        inc(Temp,sizeof(Count));
        Info:=PPointer(Temp)^;
        inc(Temp,sizeof(Info));
        { Process elements }
        for I:=0 to Count-1 do
          fpc_Copy_internal(Src+(I*size),Dest+(I*size),Info);
        Result:=size*count;
      end;
    tkobject,
    tkrecord:
      begin
        Temp:=PByte(TypeInfo);
        inc(Temp);
        { Skip Name }
        namelen:=Temp^;
        inc(temp,namelen+1);
        temp:=aligntoptr(temp);

        Result:=plongint(temp)^;

        { Skip size }
        inc(Temp,4);

        { Element count }
        Count:=PLongint(Temp)^;
        inc(Temp,sizeof(longint));
        expectedoffset:=0;
        { Process elements with rtti }
        for i:=1 to count Do
          begin
            Info:=PPointer(Temp)^;
            inc(Temp,sizeof(Info));
            Offset:=PLongint(Temp)^;
            if Offset>expectedoffset then
              move((Src+expectedoffset)^,(Dest+expectedoffset)^,Offset-expectedoffset);
            inc(Temp,sizeof(longint));
            copiedsize:=fpc_Copy_internal(Src+Offset,Dest+Offset,Info);
            expectedoffset:=Offset+copiedsize;
          end;
        { elements remaining? }
        if result>expectedoffset then
          move((Src+expectedoffset)^,(Dest+expectedoffset)^,Result-expectedoffset);
      end;
    tkDynArray:
      begin
        fpc_dynarray_Incr_Ref(PPointer(Src)^);
        fpc_dynarray_Decr_Ref(PPointer(Dest)^,typeinfo);
        PPointer(Dest)^:=PPointer(Src)^;
      end;
    tkInterface:
      begin
        Intf_Incr_Ref(PPointer(Src)^);
        Intf_Decr_Ref(PPointer(Dest)^);
        PPointer(Dest)^:=PPointer(Src)^;
      end;
    tkVariant:
      begin
        VarCopyProc(pvardata(dest)^,pvardata(src)^);
        result:=sizeof(tvardata);
      end;
  end;
end;


{ For internal use by the compiler, because otherwise $x- can cause trouble. }
{ Generally disabling extended syntax checking for all compilerprocs may     }
{ have unintended side-effects                                               }
procedure fpc_Copy_proc (Src, Dest, TypeInfo : Pointer);compilerproc; inline;
begin
  fpc_copy_internal(src,dest,typeinfo);
end;


procedure fpc_finalize_array(data,typeinfo : pointer;count,size : SizeInt); [Public,Alias:'FPC_FINALIZEARRAY'];  compilerproc;
  var
     i : SizeInt;
  begin
     if not(PByte(typeinfo)^ in [tkInteger,tkChar,tkEnumeration,tkFloat,tkSet,
       tkMethod,tkSString,tkLString,tkWChar,tkBool,tkInt64,tkQWord]) then
       for i:=0 to count-1 do
         int_finalize(data+size*i,typeinfo);
  end;

