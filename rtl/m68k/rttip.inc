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


Procedure Initialize (Data,TypeInfo : pointer);[Alias : 'FPC_INITIALIZE'];

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
    tkLstring,tkWstring : PPchar(Data)^:=Nil;
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

Procedure Finalize (Data,TypeInfo: Pointer);[Alias : 'FPC_FINALIZE'];

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
    tkLstring,tkWstring : Decr_Ansi_ref(Data);
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

Procedure Addref (Data,TypeInfo : Pointer); [alias : 'FPC_ADDREF'];

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
    tkLstring,tkWstring : Incr_Ansi_ref(Data);
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

Procedure DecRef (Data, TypeInfo : Pointer);[Public,alias : 'FPC_DECREF'];
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
    tkLstring,tkWstring : Decr_Ansi_ref(Data);
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

{
 $Log$
 Revision 1.1  2000-07-13 06:30:57  michael
 + Initial import

 Revision 1.5  2000/01/07 16:41:43  daniel
   * copyright 2000

 Revision 1.4  2000/01/07 16:32:29  daniel
   * copyright 2000 added

 Revision 1.3  1998/10/13 08:00:07  pierre
   * some bugs related to FPC_ prefix fixed
   * problems with pbyte sometimes defined and sometimes not for rttip.inc solved

 Revision 1.2  1998/06/08 19:26:53  michael
 + Implemented DecRef

 Revision 1.1  1998/06/08 15:32:08  michael
 + Split rtti according to processor. Implemented optimized i386 code.

}