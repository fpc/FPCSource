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

{ Run-Time type information routines - processor dependent part }


Procedure Initialize (Data,TypeInfo : pointer);[Alias : 'INITIALIZE'];

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
    tkRecord,tkObject,tkClass:
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

Procedure Finalize (Data,TypeInfo: Pointer);[Alias : 'FINALIZE'];

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
    tkRecord,tkObject,tkClass:
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

Procedure Addref (Data,TypeInfo : Pointer); [alias : 'ADDREF'];

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
    tkRecord,tkObject,tkClass:
      begin
      Temp:=Temp+1;
      I:=Temp^;
      temp:=temp+(I+1);             // skip name string;
      Size:=PRecRec(Temp)^.Size;    // get record size; not needed.
      Count:=PRecRec(Temp)^.Count;  // get element Count
      For I:=1 to count do
        With PRecRec(Temp)^.elements[I] do
          Addref (Data+Offset,Info);
      end;
  end;
end;

Procedure DecRef (Data, TypeInfo : Pointer);

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
    tkRecord,tkObject,tkClass:
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
  Revision 1.3  2002-09-07 16:01:26  peter
    * old logs removed and tabs fixed

}
