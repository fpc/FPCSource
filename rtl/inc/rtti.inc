{
    $Id$
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

{ A record is designed as follows :
    1    : tkrecord
    2    : Length of name string (n);
    3    : name string;
    3+n  : record size;
    7+n  : number of elements (N)
    11+n : N times : Pointer to type info
                     Offset in record
}

Type

TRecElem = Record
  Info : Pointer;
  Offset : Longint;
  end;

TRecElemArray = Array[1..Maxint] of TRecElem;

PRecRec = ^TRecRec;
TRecRec = record
  Size,Count : Longint;
  Elements : TRecElemArray;
  end;


{ An array is designed as follows :
   1    : tkArray;
   2    : length of name string (n);
   3    : NAme string
   3+n  : Element Size
   7+n  : Number of elements
   11+n : Pointer to type of elements
}

PArrayRec = ^TArrayRec;
TArrayRec = record
  Size,Count : Longint;
  Info : Pointer;
  end;


Procedure fpc_Initialize (Data,TypeInfo : pointer);saveregisters;[Public,Alias : 'FPC_INITIALIZE'];  {$ifdef hascompilerproc} compilerproc; {$endif}

{ this definition is sometimes (depending on switches)
  already defined or not so define it locally to avoid problems PM }
Type
    Pbyte = ^Byte;

Var Temp       : PByte;
    I          : longint;
    Size,Count : longint;
    TInfo : Pointer;

begin
  Temp:=PByte(TypeInfo);
  case temp^ of
    tkAstring,tkWstring,tkInterface,tkDynArray:
      PPchar(Data)^:=Nil;
    tkArray:
      begin
         inc(temp);
         I:=temp^;
         inc(temp,(I+1));                // skip name string;
         Size:=PArrayRec(Temp)^.Size;    // get element size
         Count:=PArrayRec(Temp)^.Count;  // get element Count
         TInfo:=PArrayRec(Temp)^.Info;   // Get element info
         For I:=0 to Count-1 do
           int_Initialize (Data+(I*size),TInfo);
      end;
    tkRecord,tkClass,tkObject:
      begin
         inc(Temp);
         I:=Temp^;
         inc(temp,I+1);             // skip name string;
         { if it isn't necessary, why should we load it ? FK
           Size:=PRecRec(Temp)^.Size;    // get record size; not needed.
         }
         Count:=PRecRec(Temp)^.Count;  // get element Count
         For I:=1 to count Do
           With PRecRec(Temp)^.elements[I] do
             int_Initialize (Data+Offset,Info);
      end;
{$ifdef HASVARIANT}
    tkVariant:
      variant_init(Variant(PVarData(Data)^))
{$endif HASVARIANT}
  end;
end;


Procedure fpc_finalize (Data,TypeInfo: Pointer);saveregisters;[Public,Alias : 'FPC_FINALIZE'];  {$ifdef hascompilerproc} compilerproc; {$endif}

{ this definition is sometimes (depending on switches)
  already defined or not so define it locally to avoid problems PM }
Type
    Pbyte = ^Byte;
    PPointer = ^Pointer;
Var Temp       : PByte;
    I          : longint;
    Size,Count : longint;
    TInfo : Pointer;

begin
  Temp:=PByte(TypeInfo);
  case temp^ of
    tkAstring :
      fpc_AnsiStr_Decr_Ref(PPointer(Data)^);
{$ifdef HASWIDESTRING}
    tkWstring :
      fpc_WideStr_Decr_Ref(PPointer(Data)^);
{$endif HASWIDESTRING}
    tkArray :
      begin
         inc(Temp);
         I:=temp^;
         inc(temp,I+1);                   // skip name string;
         Size:=PArrayRec(Temp)^.Size;     // get element size
         Count:=PArrayRec(Temp)^.Count;   // get element Count
         TInfo:=PArrayRec(Temp)^.Info;    // Get element info
         For I:=0 to Count-1 do
           int_Finalize (Data+(I*size),TInfo);
      end;
    tkRecord,tkObject,tkClass:
      begin
         inc(Temp);
         I:=Temp^;
         inc(temp,I+1);                // skip name string;
         { if it isn't necessary, why should we load it? FK
           Size:=PRecRec(Temp)^.Size;    // get record size; not needed.
         }
         Count:=PRecRec(Temp)^.Count;  // get element Count
         For I:=1 to count do
           With PRecRec(Temp)^.elements[I] do
             int_Finalize (Data+Offset,Info);
      end;
{$ifdef HASINTF}
    tkInterface:
      Intf_Decr_Ref(PPointer(Data)^);
{$endif HASINTF}
    tkDynArray:
      fpc_dynarray_decr_ref(PPointer(Data)^,TypeInfo);
{$ifdef HASVARIANT}
    tkVariant:
      variant_clear(Variant(PVarData(Data)^))
{$endif HASVARIANT}
  end;
end;


Procedure fpc_Addref (Data,TypeInfo : Pointer);saveregisters; [Public,alias : 'FPC_ADDREF'];  {$ifdef hascompilerproc} compilerproc; {$endif}

{ this definition is sometimes (depending on switches)
  already defined or not so define it locally to avoid problems PM }
Type
    Pbyte = ^Byte;
    PPointer = ^Pointer;
Var Temp       : PByte;
    I          : longint;
    Size,Count : longint;
    TInfo : Pointer;
begin
  Temp:=PByte(TypeInfo);
  case temp^ of
    tkAstring :
      fpc_AnsiStr_Incr_Ref(PPointer(Data)^);
{$ifdef HASWIDESTRING}
    tkWstring :
      fpc_WideStr_Incr_Ref(PPointer(Data)^);
{$endif HASWIDESTRING}
    tkArray :
      begin
        Inc(Temp);
        I:=temp^;
        inc(temp,I+1);               // skip name string;
        Size:=PArrayRec(Temp)^.Size;     // get element size
        Count:=PArrayRec(Temp)^.Count;  // get element Count
        TInfo:=PArrayRec(Temp)^.Info;   // Get element info
        For I:=0 to Count-1 do
          int_AddRef (Data+(I*size),TInfo);
      end;
    tkrecord :
      begin
        Inc(Temp);
        I:=Temp^;
        temp:=temp+(I+1);             // skip name string;
        Size:=PRecRec(Temp)^.Size;    // get record size; not needed.
        Count:=PRecRec(Temp)^.Count;  // get element Count
        For I:=1 to count do
          With PRecRec(Temp)^.elements[I] do
            int_AddRef (Data+Offset,Info);
      end;
    tkDynArray:
      fpc_dynarray_incr_ref(PPointer(Data)^);
{$ifdef HASINTF}
    tkInterface:
      Intf_Incr_Ref(PPointer(Data)^);
{$endif HASINTF}
  end;
end;


{ alias for internal use }
{ we use another name else the compiler gets puzzled because of the wrong forward def }
procedure fpc_systemDecRef (Data, TypeInfo : Pointer);saveregisters;[external name 'FPC_DECREF'];

Procedure fpc_DecRef (Data, TypeInfo : Pointer);saveregisters;[Public,alias : 'FPC_DECREF'];  {$ifdef hascompilerproc} compilerproc; {$endif}
{ this definition is sometimes (depending on switches)
  already defined or not so define it locally to avoid problems PM }
Type
    Pbyte = ^Byte;
    PPointer = ^Pointer;
Var Temp       : PByte;
    I          : longint;
    Size,Count : longint;
    TInfo : Pointer;

begin
  Temp:=PByte(TypeInfo);
  case temp^ of
    { see AddRef for comment about below construct (JM) }
    tkAstring:
      fpc_AnsiStr_Decr_Ref(PPointer(Data)^);
{$ifdef HASWIDESTRING}      
    tkWstring:
      fpc_WideStr_Decr_Ref(PPointer(Data)^);
{$endif HASWIDESTRING}      
    tkArray:
      begin
         inc(Temp);
         I:=temp^;
         inc(temp,I+1);               // skip name string;
         Size:=PArrayRec(Temp)^.Size;     // get element size
         Count:=PArrayRec(Temp)^.Count;  // get element Count
         TInfo:=PArrayRec(Temp)^.Info;   // Get element info
         For I:=0 to Count-1 do
           fpc_systemDecRef (Data+(I*size),TInfo);
      end;
    tkrecord:
      begin
      Temp:=Temp+1;
      I:=Temp^;
      temp:=temp+(I+1);             // skip name string;
      Size:=PRecRec(Temp)^.Size;    // get record size; not needed.
      Count:=PRecRec(Temp)^.Count;  // get element Count
      For I:=1 to count do
        With PRecRec(Temp)^.elements[I] do
          fpc_systemDecRef (Data+Offset,Info);
      end;
    tkDynArray:
      fpc_dynarray_decr_ref(PPointer(Data)^,TypeInfo);
{$ifdef HASINTF}
    tkInterface:
      Intf_Decr_Ref(PPointer(Data)^);
{$endif HASINTF}
  end;
end;


procedure fpc_finalize_array(data,typeinfo : pointer;count,size : longint); [Public,Alias:'FPC_FINALIZEARRAY'];  {$ifdef hascompilerproc} compilerproc; {$endif}
  var
     i : longint;
  begin
     for i:=0 to count-1 do
       int_finalize(data+size*i,typeinfo);
  end;



{
  $Log$
  Revision 1.6  2002-09-02 18:42:41  peter
    * moved genrtti.inc code to rtti
    * removed rttip.inc, the generic code is almost as fast and
      much easier to maintain and has less risks on bugs

  Revision 1.5  2001/11/17 16:56:08  florian
    * init and final code in genrtti.inc updated

  Revision 1.4  2001/04/23 18:25:45  peter
    * m68k updates

  Revision 1.3  2000/10/21 18:20:17  florian
    * a lot of small changes:
       - setlength is internal
       - win32 graph unit extended
       ....

  Revision 1.2  2000/07/13 11:33:45  michael
  + removed logs

}
