{
    $Id$
    This file is part of the Free Pascal run time library.
    Copyright (c) 1993,97 by xxxx
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
  tkLString  = 10;
  tkWString = 11;
  tkVariant  = 12;
  tkArray    = 13;
  tkRecord   = 14;
 
{ Some useful types }
Type

  PByte = ^Byte;


{ A record is designed as follows : 
    1    : tkrecord
    2    : Length of name string (n);
    3    : name string;
    3+n  : record size;
    7+n  : number of elements (N)
    11+n : N times : Pointer to type info
                     Offset in record
} 

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
