{
    $Id$
    This file is part of the Free Pascal run time library.
    Copyright (c) 1999-2000 by the Free Pascal development team

    Interface and OS-dependent part of variant support
       
    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}

{$MODE ObjFPC}

Unit varutils;

Interface

Uses sysutils;

// Read definitions.

{$i varutilh.inc}

Implementation

// Code common to all platforms.

{$i cvarutil.inc}

// Code common to non-win32 platforms.

{$i varutils.inc}

end.

{
  $Log$
  Revision 1.1  2000-08-29 18:21:58  michael
  + new include files

  Revision 1.1  2000/08/29 18:20:13  michael
  + new include files

}

