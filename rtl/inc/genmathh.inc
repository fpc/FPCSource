{
    This file is part of the Free Pascal run time library.
    Copyright (c) 1999-2007 by Several contributors

    Generic mathematical routines (on type real)

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}
{*************************************************************************}
{  Credits                                                                }
{*************************************************************************}
{       Copyright Abandoned, 1987, Fred Fish                              }
{                                                                         }
{       This previously copyrighted work has been placed into the         }
{       public domain by the author (Fred Fish) and may be freely used    }
{       for any purpose, private or commercial.  I would appreciate       }
{       it, as a courtesy, if this notice is left in all copies and       }
{       derivative works.  Thank you, and enjoy...                        }
{                                                                         }
{       The author makes no warranty of any kind with respect to this     }
{       product and explicitly disclaims any implied warranties of        }
{       merchantability or fitness for any particular purpose.            }
{-------------------------------------------------------------------------}
{       Copyright (c) 1992 Odent Jean Philippe                            }
{                                                                         }
{       The source can be modified as long as my name appears and some    }
{       notes explaining the modifications done are included in the file. }
{-------------------------------------------------------------------------}
{       Copyright (c) 1997 Carl Eric Codere                               }
{-------------------------------------------------------------------------}
{-------------------------------------------------------------------------
 Using functions from AMath/DAMath libraries, which are covered by the
 following license:

 (C) Copyright 2009-2013 Wolfgang Ehrhardt

 This software is provided 'as-is', without any express or implied warranty.
 In no event will the authors be held liable for any damages arising from
 the use of this software.

 Permission is granted to anyone to use this software for any purpose,
 including commercial applications, and to alter it and redistribute it
 freely, subject to the following restrictions:

 1. The origin of this software must not be misrepresented; you must not
    claim that you wrote the original software. If you use this software in
    a product, an acknowledgment in the product documentation would be
    appreciated but is not required.

 2. Altered source versions must be plainly marked as such, and must not be
    misrepresented as being the original software.

 3. This notice may not be removed or altered from any source distribution.
----------------------------------------------------------------------------}

{$ifndef FPC_SYSTEM_HAS_float64}
type
{ also necessary for Int() on systems with 64bit floats (JM) }
{ case record required to get correct alignement for floating point type }
{$ifdef ENDIAN_LITTLE}
  float64 = record
    case byte of
      1: (dummy : double);
 {$ifndef FPC_DOUBLE_HILO_SWAPPED}
      2: (low,high: longword);
{$else}
      2: (high,low: longword);
{$endif FPC_DOUBLE_HILO_SWAPPED}
  end;
{$else}
  float64 = record
    case byte of
      1: (dummy : double);
{$ifndef FPC_DOUBLE_HILO_SWAPPED}
      2: (high,low: longword);
{$else}
      2: (low,high: longword);
{$endif FPC_DOUBLE_HILO_SWAPPED}
  end;
{$endif}
{$define FPC_SYSTEM_HAS_float64}
{$endif FPC_SYSTEM_HAS_float64}

{$ifndef FPC_SYSTEM_HAS_float32}
type
  float32 = longword;
{$define FPC_SYSTEM_HAS_float32}
{$endif FPC_SYSTEM_HAS_float32}

