{
    $Id$
    This file is part of the Free Pascal run time library.
    Copyright (c) 1999-2001 by the Free Pascal development team

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}

{$ifdef HASINTF}

type
   IReadWriteSync = interface
      procedure BeginRead;
      procedure EndRead;
      procedure BeginWrite;
      procedure EndWrite;
   end;

{$endif HASINTF}

{
  $Log$
  Revision 1.1  2001-08-22 21:19:16  florian
    + some new stuff of D6/Kylix added

}
