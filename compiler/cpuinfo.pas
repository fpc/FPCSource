{
    $Id$
    This file is part of the Free Pascal run time library.
    Copyright (c) 1999 by the Free Pascal development team

    Basic Processor information

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}

Unit CPUInfo;

Interface

Type
{$ifdef FPC}
   AWord = dword;
{$else FPC}
   AWord = Longint;
{$endif FPC}

Const
   { Size of native extended type }
   extended_size = 10;
   
Implementation

end.
{
   $Log$
   Revision 1.1  1999-08-04 11:13:38  florian
     + initial revision

}
