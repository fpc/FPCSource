{
    $Id$
    This file is part of the Free Pascal run time library.
    Copyright (c) 1999-2000 by xxxx
    member of the Free Pascal development team

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}

{ Run-Time type information routines - processor dependent part }


{$ifndef FPC_SYSTEM_HAS_FPC_INITIALIZE}
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
{$endif}

{$ifndef FPC_SYSTEM_HAS_FPC_FINALIZE}

Procedure fpc_Finalize (Data,TypeInfo: Pointer);saveregisters;[Public,Alias : 'FPC_FINALIZE'];  {$ifdef hascompilerproc} compilerproc; {$endif}

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
    tkAstring,tkWstring:
      fpc_AnsiStr_Decr_Ref(PPointer(Data)^);
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
{$endif}

{$ifndef FPC_SYSTEM_HAS_FPC_ADDREF}

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
    { In case of an ansistring, data is pushed as a var parameter.   }
    { This means that if you look at data as a value parameter, it   }
    { containst the address of the ansistring. AnsiStr_Incr_Ref also }
    { expects a var parameter, so to pass the address of the         }
    { ansistring and not that of the data parameter on the stack,    }
    { you have to dereference data (JM)                              }
    tkAstring,tkWstring : fpc_AnsiStr_Incr_Ref(PPointer(Data)^);
    tkArray :
      begin
      Temp:=Temp+1;
      I:=temp^;
      temp:=temp+(I+1);               // skip name string;
      Size:=PArrayRec(Temp)^.Size;     // get element size
      Count:=PArrayRec(Temp)^.Count;  // get element Count
      TInfo:=PArrayRec(Temp)^.Info;   // Get element info
      For I:=0 to Count-1 do
        int_AddRef (Data+(I*size),TInfo);
      end;
    tkrecord :
      begin
      Temp:=Temp+1;
      I:=Temp^;
      temp:=temp+(I+1);             // skip name string;
      Size:=PRecRec(Temp)^.Size;    // get record size; not needed.
      Count:=PRecRec(Temp)^.Count;  // get element Count
      For I:=1 to count do
        With PRecRec(Temp)^.elements[I] do
          int_AddRef (Data+Offset,Info);
      end;
    tkDynArray:
      fpc_dynarray_incr_ref(PPointer(Data)^,TypeInfo);
{$ifdef HASINTF}
    tkInterface:
      Intf_Incr_Ref(PPointer(Data)^);
{$endif HASINTF}
  end;
end;
{$endif}


{$ifndef FPC_SYSTEM_HAS_FPC_DECREF}

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
    tkAstring,tkWstring:
      fpc_AnsiStr_Decr_Ref(PPointer(Data)^);
    tkArray:
      begin
         inc(Temp);
         I:=temp^;
         inc(temp,I+1);               // skip name string;
         Size:=PArrayRec(Temp)^.Size;     // get element size
         Count:=PArrayRec(Temp)^.Count;  // get element Count
         TInfo:=PArrayRec(Temp)^.Info;   // Get element info
         For I:=0 to Count-1 do
           fpc_DecRef (Data+(I*size),TInfo);
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
          fpc_DecRef (Data+Offset,Info);
      end;
    tkDynArray:
      fpc_dynarray_decr_ref(PPointer(Data)^,TypeInfo);
{$ifdef HASINTF}
    tkInterface:
      Intf_Decr_Ref(PPointer(Data)^);
{$endif HASINTF}
  end;
end;
{$endif}

{$ifndef FPC_SYSTEM_HAS_FPC_FINALIZEARRAY}
procedure fpc_FinalizeArray(data,typeinfo : pointer;count,size : longint); [Public,Alias:'FPC_FINALIZEARRAY'];  {$ifdef hascompilerproc} compilerproc; {$endif}
  var
     i : longint;
  begin
     for i:=0 to count-1 do
       int_finalize(data+size*i,typeinfo);
  end;
{$endif}

{
 $Log$
 Revision 1.10  2001-11-30 16:25:35  jonas
   * fixed web bug 1707:
      * tvarsym.getvaluesize doesn't return 0 anymore for dynarrays (found
        by Florian)
      * in genrtti, some more ppointer(data)^ tricks were necessary

 Revision 1.9  2001/11/22 07:33:08  michael
  * Fixed memory corruption with finalize() of ansistring in a class

 Revision 1.8  2001/11/17 16:56:08  florian
   * init and final code in genrtti.inc updated

 Revision 1.7  2001/11/17 10:29:48  florian
   * make cycle for win32 fixed

 Revision 1.6  2001/11/14 22:59:11  michael
   + Initial variant support

 Revision 1.5  2001/08/01 15:00:10  jonas
   + "compproc" helpers
   * renamed several helpers so that their name is the same as their
     "public alias", which should facilitate the conversion of processor
     specific code in the code generator to processor independent code
   * some small fixes to the val_ansistring and val_widestring helpers
     (always immediately exit if the source string is longer than 255
      chars)
   * fixed fpc_dynarray_high and fpc_dynarray_length if the dynarray is
     still nil (used to crash, now return resp -1 and 0)

 Revision 1.4  2001/06/28 19:18:57  peter
   * ansistr fix merged

 Revision 1.3  2001/05/28 20:43:17  peter
   * more saveregisters added (merged)

 Revision 1.2  2001/04/23 18:25:44  peter
   * m68k updates

}
