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
  tkLString  = 10;
  tkWString = 11;
  tkVariant  = 12;
  tkArray    = 13;
  tkRecord   = 14;
 
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
  
{ The actual Routines are implemented per processor. }

{$i rttip.inc}

{ 
  $Log$
  Revision 1.5  2000-01-07 16:41:36  daniel
    * copyright 2000

  Revision 1.4  1998/07/20 18:42:51  florian
  *** empty log message ***

  Revision 1.3  1998/07/13 21:19:11  florian
    * some problems with ansi string support fixed

  Revision 1.2  1998/06/08 15:32:15  michael
  + Split rtti according to processor. Implemented optimized i386 code.

}
