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
  Revision 1.1  2000-07-13 06:30:48  michael
  + Initial import

  Revision 1.8  2000/06/22 20:04:07  peter
    * added tkint64,tkqword constants

  Revision 1.7  2000/06/14 08:52:05  michael
  + Fixed all constants so they match compiler defined constants

  Revision 1.6  2000/02/09 16:59:31  peter
    * truncated log

  Revision 1.5  2000/01/07 16:41:36  daniel
    * copyright 2000

}