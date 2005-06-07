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



Procedure fpc_Initialize (Data,TypeInfo : pointer);{$ifndef NOSAVEREGISTERS}saveregisters;{$endif}[Public,Alias : 'FPC_INITIALIZE'];  {$ifdef hascompilerproc} compilerproc; {$endif}
begin
  case PByte(TypeInfo)^ of
    tkAstring,tkWstring,tkInterface,tkDynArray:
      PPchar(Data)^:=Nil;
    tkArray:
      arrayrtti(data,typeinfo,@int_initialize);
    tkObject,
    tkRecord:
      recordrtti(data,typeinfo,@int_initialize);
{$ifdef HASVARIANT}
    tkVariant:
      variant_init(PVarData(Data)^);
{$endif HASVARIANT}
  end;
end;


Procedure fpc_finalize (Data,TypeInfo: Pointer);{$ifndef NOSAVEREGISTERS}saveregisters;{$endif}[Public,Alias : 'FPC_FINALIZE'];  {$ifdef hascompilerproc} compilerproc; {$endif}
begin
  case PByte(TypeInfo)^ of
    tkAstring :
      begin
       fpc_AnsiStr_Decr_Ref(PPointer(Data)^);
        PPointer(Data)^:=nil;
      end;
{$ifdef HASWIDESTRING}
    tkWstring :
      begin
        fpc_WideStr_Decr_Ref(PPointer(Data)^);
        PPointer(Data)^:=nil;
      end;
{$endif HASWIDESTRING}
    tkArray :
      arrayrtti(data,typeinfo,@int_finalize);
    tkObject,
    tkRecord:
      recordrtti(data,typeinfo,@int_finalize);
{$ifdef HASINTF}
    tkInterface:
      begin
        Intf_Decr_Ref(PPointer(Data)^);
        PPointer(Data)^:=nil;
      end;
{$endif HASINTF}
    tkDynArray:
      fpc_dynarray_decr_ref(PPointer(Data)^,TypeInfo);
{$ifdef HASVARIANT}
    tkVariant:
      variant_clear(PVarData(Data)^);
{$endif HASVARIANT}
  end;
end;


Procedure fpc_Addref (Data,TypeInfo : Pointer);{$ifndef NOSAVEREGISTERS}saveregisters;{$endif} [Public,alias : 'FPC_ADDREF'];  {$ifdef hascompilerproc} compilerproc; {$endif}
begin
  case PByte(TypeInfo)^ of
    tkAstring :
      fpc_AnsiStr_Incr_Ref(PPointer(Data)^);
{$ifdef HASWIDESTRING}
    tkWstring :
      fpc_WideStr_Incr_Ref(PPointer(Data)^);
{$endif HASWIDESTRING}
    tkArray :
      arrayrtti(data,typeinfo,@int_addref);
    tkobject,
    tkrecord :
      recordrtti(data,typeinfo,@int_addref);
    tkDynArray:
      fpc_dynarray_incr_ref(PPointer(Data)^);
{$ifdef HASINTF}
    tkInterface:
      Intf_Incr_Ref(PPointer(Data)^);
{$endif HASINTF}
{$ifdef HASVARIANT}
    tkVariant:
      variant_addref(pvardata(Data)^);
{$endif HASVARIANT}
  end;
end;


{ alias for internal use }
{ we use another name else the compiler gets puzzled because of the wrong forward def }
procedure fpc_systemDecRef (Data, TypeInfo : Pointer);{$ifndef NOSAVEREGISTERS}saveregisters;{$endif}[external name 'FPC_DECREF'];

Procedure fpc_DecRef (Data, TypeInfo : Pointer);{$ifndef NOSAVEREGISTERS}saveregisters;{$endif}[Public,alias : 'FPC_DECREF'];  {$ifdef hascompilerproc} compilerproc; {$endif}
begin
  case PByte(TypeInfo)^ of
    { see AddRef for comment about below construct (JM) }
    tkAstring:
      fpc_AnsiStr_Decr_Ref(PPointer(Data)^);
{$ifdef HASWIDESTRING}
    tkWstring:
      fpc_WideStr_Decr_Ref(PPointer(Data)^);
{$endif HASWIDESTRING}
    tkArray:
      arrayrtti(data,typeinfo,@fpc_systemDecRef);
    tkobject,
    tkrecord:
      recordrtti(data,typeinfo,@fpc_systemDecRef);
    tkDynArray:
      fpc_dynarray_decr_ref(PPointer(Data)^,TypeInfo);
{$ifdef HASINTF}
    tkInterface:
      Intf_Decr_Ref(PPointer(Data)^);
{$endif HASINTF}
{$ifdef HASVARIANT}
    tkVariant:
      variant_clear(pvardata(data)^);
{$endif HASVARIANT}
  end;
end;


procedure fpc_finalize_array(data,typeinfo : pointer;count,size : longint); [Public,Alias:'FPC_FINALIZEARRAY'];  {$ifdef hascompilerproc} compilerproc; {$endif}
  var
     i : longint;
  begin
     for i:=0 to count-1 do
       int_finalize(data+size*i,typeinfo);
  end;

