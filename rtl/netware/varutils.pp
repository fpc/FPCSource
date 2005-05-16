{
    $Id: varutils.pp,v 1.3 2005/02/14 17:13:30 peter Exp $
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
  $Log: varutils.pp,v $
  Revision 1.3  2005/02/14 17:13:30  peter
    * truncate log

}

