{
    $Id: emuld.inc,v 1.2 2005/02/14 17:13:30 peter Exp $
    This file is part of the Free Pascal run time library.

    emulinterface definitions for MorphOS/PowerPC
    Copyright (c) 2002 The MorphOS Development Team, All Rights Reserved.

    Free Pascal conversion
    Copyright (c) 2004 Karoly Balogh for Genesi S.a.r.l. <www.genesi.lu>

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}

{ * emulinterface structures from MorphOS SDK * }

type
  PEmulLibEntry = ^TEmulLibEntry;
  TEmulLibEntry = packed record
    Trap     : Word;
    Extension: Word;    { * MUST be set to 0 if you create it by hand * }
    Func     : Pointer;
  end;

{
  $Log: emuld.inc,v $
  Revision 1.2  2005/02/14 17:13:30  peter
    * truncate log

}
