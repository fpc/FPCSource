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
Procedure Initialize (Data,TypeInfo : pointer);saveregisters;[Public,Alias : 'FPC_INITIALIZE'];

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
    tkAstring,tkWstring : PPchar(Data)^:=Nil;
    tkArray :
      begin
      temp:=Temp+1;
      I:=temp^;
      temp:=temp+(I+1);               // skip name string;
      Size:=PArrayRec(Temp)^.Size;     // get element size
      Count:=PArrayRec(Temp)^.Count;  // get element Count
      TInfo:=PArrayRec(Temp)^.Info;   // Get element info
      For I:=0 to Count-1 do
        Initialize (Data+(I*size),TInfo);
      end;
    tkrecord :
      begin
      Temp:=Temp+1;
      I:=Temp^;
      temp:=temp+(I+1);             // skip name string;
      Size:=PRecRec(Temp)^.Size;    // get record size; not needed.
      Count:=PRecRec(Temp)^.Count;  // get element Count
      For I:=1 to count Do
        With PRecRec(Temp)^.elements[I] do
          Initialize (Data+Offset,Info);
      end;
  end;
end;
{$endif}

{$ifndef FPC_SYSTEM_HAS_FPC_FINALIZE}
Procedure Finalize (Data,TypeInfo: Pointer);saveregisters;[Public,Alias : 'FPC_FINALIZE'];

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
    tkAstring,tkWstring : AnsiStr_Decr_Ref(Data);
    tkArray :
      begin
      Temp:=Temp+1;
      I:=temp^;
      temp:=temp+(I+1);               // skip name string;
      Size:=PArrayRec(Temp)^.Size;     // get element size
      Count:=PArrayRec(Temp)^.Count;  // get element Count
      TInfo:=PArrayRec(Temp)^.Info;   // Get element info
      For I:=0 to Count-1 do
        Finalize (Data+(I*size),TInfo);
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
          Finalize (Data+Offset,Info);
      end;
  end;
end;
{$endif}

{$ifndef FPC_SYSTEM_HAS_FPC_ADDREF}
Procedure Addref (Data,TypeInfo : Pointer);saveregisters; [Public,alias : 'FPC_ADDREF'];

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
    tkAstring,tkWstring : AnsiStr_Incr_Ref(Data);
    tkArray :
      begin
      Temp:=Temp+1;
      I:=temp^;
      temp:=temp+(I+1);               // skip name string;
      Size:=PArrayRec(Temp)^.Size;     // get element size
      Count:=PArrayRec(Temp)^.Count;  // get element Count
      TInfo:=PArrayRec(Temp)^.Info;   // Get element info
      For I:=0 to Count-1 do
        AddRef (Data+(I*size),TInfo);
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
          AddRef (Data+Offset,Info);
      end;
  end;
end;
{$endif}

{$ifndef FPC_SYSTEM_HAS_FPC_DECREF}
Procedure DecRef (Data, TypeInfo : Pointer);saveregisters;[Public,alias : 'FPC_DECREF'];
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
    tkAstring,tkWstring : AnsiStr_Decr_Ref(Data);
    tkArray :
      begin
      Temp:=Temp+1;
      I:=temp^;
      temp:=temp+(I+1);               // skip name string;
      Size:=PArrayRec(Temp)^.Size;     // get element size
      Count:=PArrayRec(Temp)^.Count;  // get element Count
      TInfo:=PArrayRec(Temp)^.Info;   // Get element info
      For I:=0 to Count-1 do
        DecRef (Data+(I*size),TInfo);
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
          DecRef (Data+Offset,Info);
      end;
  end;
end;
{$endif}

{$ifndef FPC_SYSTEM_HAS_FPC_FINALIZEARRAY}
procedure FinalizeArray(data,typeinfo : pointer;count,size : longint); [Public,Alias:'FPC_FINALIZEARRAY'];
  var
     i : longint;
  begin
     for i:=0 to count-1 do
       int_finalize(data+size*i,typeinfo);
  end;
{$endif}

{
 $Log$
 Revision 1.3  2001-05-28 20:43:17  peter
   * more saveregisters added (merged)

 Revision 1.2  2001/04/23 18:25:44  peter
   * m68k updates

}