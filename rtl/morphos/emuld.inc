{
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

{ * emulinterface consts from MorphOS SDK * }

const
  TRAP_MASK            = $00ff;
  TRAP_AREA_START      = $ff00;      { * $ff00 .. $ffff area * }
  TRAP_LIB             = $ff00;
  TRAP_LIB_QUICK       = $ff01;
  TRAP_LIBNR           = $ff05;
  TRAP_LIBNR_QUICK     = $ff06;
  TRAP_ISYNC           = $ff0f;
  TRAP_SYNC            = $ff10;
  TRAP_EIEIO           = $ff11;
  TRAP_LIBSR           = $ff12;
  TRAP_LIBSRNR         = $ff13;
  TRAP_LIBD0_D1        = $ff14;
  TRAP_LIBRESTORE      = $ff15;
  TRAP_LIBD0D1SR       = $ff17;
  TRAP_LIBD0D1A0A1SR   = $ff18;

const
  EMULTAG_NAME             = $0;    { * Gives back a Name Ptr * }
  EMULTAG_VERSION          = $1;    { * Gives back a Version  * }
  EMULTAG_REVISION         = $2;    { * Gives back a Revision * }
  EMULTAG_OPCODETABLE      = $3;    { * Gives back the 16 Bit Opcodetable Ptr or NULL * }
  EMULTAG_TYPE             = $4;    { * Gives back the emulation type   * }
  EMULTAG_EMULHANDLE       = $5;    { * Gives back the EmulHandle Ptr   * }
  EMULTAG_EMULHANDLESIZE   = $6;    { * Gives back the EmulHandle Size  * }
  EMULTAG_SUPERHANDLE      = $7;    { * Gives back the SuperHandle Ptr  * }
  EMULTAG_SUPERHANDLESIZE  = $8;    { * Gives back the SuperHandle Size * }

{ * emulinterface structures from MorphOS SDK * }

type
  PEmulLibEntry = ^TEmulLibEntry;
  TEmulLibEntry = packed record
    Trap     : Word;
    Extension: Word;    { * MUST be set to 0 if you create it by hand * }
    Func     : Pointer;
  end;

