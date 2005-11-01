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


type
  TRTTIProc=procedure(Data,TypeInfo:Pointer);

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
    tkAstring,tkWstring,tkInterface,tkDynArray:
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
    tkWstring :
      begin
        fpc_WideStr_Decr_Ref(PPointer(Data)^);
        PPointer(Data)^:=nil;
      end;
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
    tkWstring :
      fpc_WideStr_Incr_Ref(PPointer(Data)^);
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
    tkWstring:
      fpc_WideStr_Decr_Ref(PPointer(Data)^);
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


{
Procedure fpc_Copy (Src, Dest, TypeInfo : Pointer);[Public,alias : 'FPC_COPY'];  compilerproc;
var
  Temp : pbyte;
  namelen : byte;
  count,
  offset,
  i : longint;
  info : pointer;
begin
  case PByte(TypeInfo)^ of
    tkAstring:
      begin
        fpc_AnsiStr_Incr_Ref(PPointer(Src)^);
        fpc_AnsiStr_Decr_Ref(PPointer(Dest)^);
        PPointer(Dest)^:=PPointer(Src)^;
      end;
    tkWstring:
      begin
        fpc_WideStr_Incr_Ref(PPointer(Src)^);
        fpc_WideStr_Decr_Ref(PPointer(Dest)^);
      end;
    tkArray:
      begin
        arrayrtti(data,typeinfo,@fpc_systemDecRef);
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

        { copy data }
        move(src^,dest^,plongint(temp)^);

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
            fpc_Copy(Src+Offset,Src+Offset,Info);
	  end;
    tkDynArray:
      begin
        fpc_dynarray_Incr_Ref(PPointer(Src)^);
        fpc_dynarray_Decr_Ref(PPointer(Dest)^);
        PPointer(Dest)^:=PPointer(Src)^;
      end;
    tkInterface:
      begin
        Intf_Incr_Ref(PPointer(Src)^);
        Intf_Decr_Ref(PPointer(Dest)^);
        PPointer(Dest)^:=PPointer(Src)^;
      end;
    tkVariant:
      VarCopyProc(pvardata(dest)^,pvardata(src)^);
  end;
end;
}


procedure fpc_finalize_array(data,typeinfo : pointer;count,size : longint); [Public,Alias:'FPC_FINALIZEARRAY'];  compilerproc;
  var
     i : longint;
  begin
     for i:=0 to count-1 do
       int_finalize(data+size*i,typeinfo);
  end;

