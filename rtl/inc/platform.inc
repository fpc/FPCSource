{*****************************************************************************
   Include file to sort out compilers/platforms/targets

   Copyright (c) 1997 Balazs Scheidler (bazsi@tas.vein.hu)

   This library is free software; you can redistribute it and/or
   modify it under the terms of the GNU Library General Public
   License as published by the Free Software Foundation; either
   version 2 of the License, or (at your option) any later version.


   This library is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
   Library General Public License for more details.

   You should have received a copy of the GNU Library General Public
   License along with this library; if not, write to the Free
   Software Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.


 *****************************************************************************

   This include file defines some conditional defines to allow us to select
   the compiler/platform/target in a consequent way.

    OS_XXXX         The operating system used (XXXX may be one of:
                       DOS, OS2, Linux, Windows)

 *****************************************************************************

   Changelog:

     Date       Version        Who        Comments
     02 Jul 97  0.1            Bazsi      Initial implementation
     28 Aug 97  0.2            LdeB       Fixed OS2 platform sort out
     29 Aug 97  0.3            LdeB       Added assembler type change
     29 Aug 97  0.4            LdeB       OS_DOS removed from Windows
      5 May 98  0.5            CEC        FPC only support - fixed for Win32
 *****************************************************************************

    This is how the IFDEF and UNDEF statements below should translate.


 PLATFORM  SYSTEM    COMPILER  HANDLE SIZE      ASM          CPU
 --------  ------    --------  -----------      ----         ---

 DOS      OS_DOS       FPC     16-bit           AT&T         CPU86

 WIN32    OS_WINDOWS   FPC     32-bit           AT&T         ----

 LINUX    OS_LINUX     FPC     32-bit           AT&T         ----

 OS2      OS_OS2       FPC     ?????            AT&T         CPU86

 ATARI    OS_ATARI     FPC     16-bit           Internal     CPU68

 MACOS    OS_MAC       FPC     ?????            Internal     CPU68

 AMIGA    OS_AMIGA     FPC     32-bit           Internal     CPU68

 *****************************************************************************}

{$IFDEF FPC}

{$IFDEF GO32V1}
{$I386_ATT}
{$IFNDEF CPU86}
  {$DEFINE CPU86}
{$ENDIF}
{$DEFINE OS_DOS}
{$ENDIF}

{$IFDEF GO32V2}
{$I386_ATT}
{$IFNDEF CPU86}
  {$DEFINE CPU86}
{$ENDIF}
{$DEFINE OS_DOS}
{$ENDIF}

{$IFDEF LINUX}
{$DEFINE OS_LINUX}
{$ENDIF}

{$IFDEF WIN32}
{$DEFINE OS_WINDOWS}
{$ENDIF}

{$IFDEF OS2}
{$I386_ATT}
{$IFNDEF CPU86}
  {$DEFINE CPU86}
{$ENDIF}
{$DEFINE OS_OS2}
{$ENDIF}

{$IFDEF AMIGA}
{$DEFINE OS_AMIGA}
{$IFNDEF CPU68}
  {$DEFINE CPU68}
{$ENDIF}
{$ENDIF}

{$IFDEF ATARI}
{$DEFINE OS_ATARI}
{$IFNDEF CPU68}
  {$DEFINE CPU68}
{$ENDIF}
{$ENDIF}

{$IFDEF MACOS}
{$DEFINE OS_MAC}
{$IFNDEF CPU68}
  {$DEFINE CPU68}
{$ENDIF}
{$ENDIF}

{$ELSE}
Requires Free Pascal (FPK) v0.9.2 or higher
{$ENDIF}

