{
   $Id$
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

 ****************************************************************************

   This include file defines some conditional defines to allow us to select
   the compiler/platform/target in a consequent way.

    OS_XXXX         The operating system used (XXXX may be one of:
                       DOS, OS2, Linux, Windows, Go32)
    PPC_XXXX        The compiler used: BP, FPK, Virtual, Speed
    BIT_XX          The number of bits of the target platform: 16 or 32
    PROC_XXXX       The mode of the target processor (Real or Protected)
                    This shouldn't be used, except for i386 specific parts.
    ASM_XXXX        This is the assembler type: BP, ISO-ANSI, FPK

 ****************************************************************************

   Changelog:

     Date       Version        Who        Comments
     02 Jul 97  0.1            Bazsi      Initial implementation
     28 Aug 97  0.2            LdeB       Fixed OS2 platform sort out
     29 Aug 97  0.3            LdeB       Added assembler type change
     29 Aug 97  0.4            LdeB       OS_DOS removed from Windows
     23 Oct 97  0.5            LdeB       Delphi & Speed compilers added
     05 May 98  0.6            LdeB       Virtual pascal 2.0 added
     19 May 98  0.7            LdeB       Delphi2/3 definitions altered
      6 Aug 98  0.8            CEC/LdeB   FPC only support - fixed for Win32
     10 Aug 98  0.9            LdeB       BP_VMTLink def/Undef for object reg.
     27 Aug 98  1.0            LdeB       Fixed Atari etc not $UNDEF OS_DOS.

     25 Oct 98  1.1            pfv        Delphi4

 ****************************************************************************

    This is how the IFDEF and UNDEF statements below should translate.


 PLATFORM  SYSTEM    COMPILER  COMP ID      CPU MODE        BITS    ASSEMBLER
 --------  ------    --------  -------      --------        ----    ---------

 DOS      OS_DOS      BP/TP7   PPC_BP       PROC_Real       BIT_16  ASM_BP
                      FPC      PPC_FPC      PROC_Protected  BIT_32  ASM_FPC

 DPMI     OS_DOS      BP/TP7   PPC_BP       PROC_Protected  BIT_16  ASM_BP

 LINUX    OS_LINUX    FPC      PPC_FPC      PROC_Protected  BIT_32  ASM_FPC

 WINDOWS  OS_WINDOWS  BP/TP7   PPC_BP       PROC_Protected  BIT_16  ASM_BP
                      DELPHI   PPC_DELPHI   PROC_Protected  BIT_16  ASM_BP
                      DELPHI2  PPC_DELPHI&2 PROC_Protected  BIT_16  ASM_BP

 WIN95/NT OS_WINDOWS  DELPHI2  PPC_DELPHI&2 PROC_Protected  BIT_32  ASM_BP
                      DELPHI3  PPC_DELPHI&3 PROC_Protected  BIT_32  ASM_BP
                      DELPHI4  PPC_DELPHI&3 PROC_Protected  BIT_32  ASM_BP
                      VIRTUAL  PPC_VIRTUAL  PROC_Protected  BIT 32  ASM_BP

 OS2      OS_OS2      BPOS2    PPC_BPOS2    PROC_Protected  BIT_16  ASM_BP
                      VIRTUAL  PPC_VIRTUAL  PROC_Protected  BIT_32  ASM_BP
                      SPEED    PPC_SPEED    PROC_Protected  BIT_32  ASM_BP
                      FPC      PPC_FPC      PROC_Protected  BIT_32  ASM_FPC
 ****************************************************************************}
{****************************************************************************

FOR ALL COMPILERS BP_VMTLink will be defined but FPC and Delphi3 undefine it

 ****************************************************************************}
{****************************************************************************

FOR FPC THESE ARE THE TRANSLATIONS

  PLATFORM  SYSTEM    COMPILER  HANDLE SIZE      ASM          CPU
 --------  ------    --------  -----------      ----         ---

 DOS      OS_DOS,OS_GO32 FPC     32-bit           AT&T         CPU86

 WIN32    OS_WINDOWS   FPC     32-bit           AT&T         ----

 LINUX    OS_LINUX     FPC     32-bit           AT&T         ----

 OS2      OS_OS2       FPC     ?????            AT&T         CPU86

 ATARI    OS_ATARI     FPC     32-bit           Internal     CPU68

 MACOS    OS_MAC       FPC     ?????            Internal     CPU68

 AMIGA    OS_AMIGA     FPC     32-bit           Internal     CPU68

 ****************************************************************************}

{---------------------------------------------------------------------------}
{  Initial assume BORLAND 16 BIT DOS COMPILER - Updated 27Aug98 LdB         }
{---------------------------------------------------------------------------}
{$DEFINE OS_DOS}
{$DEFINE PROC_Real}
{$DEFINE BIT_16}
{$DEFINE PPC_BP}
{$DEFINE ASM_BP}
{$DEFINE BP_VMTLink}

{---------------------------------------------------------------------------}
{  BORLAND 16 BIT DPMI changes protected mode - Updated 27Aug98 LdB         }
{---------------------------------------------------------------------------}
{$IFDEF DPMI}
  {$UNDEF PROC_Real}
  {$DEFINE PROC_Protected}
{$ENDIF}

{---------------------------------------------------------------------------}
{  FPC 32 BIT COMPILER changes ASM, 32 bits etc - Updated 27Aug98 LdB       }
{---------------------------------------------------------------------------}
{$IFDEF FPC}
  {$UNDEF PROC_Real}
  {$DEFINE PROC_Protected}
  {$UNDEF BIT_16}
  {$DEFINE BIT_32}
  {$UNDEF PPC_BP}
  {$DEFINE PPC_FPC}
  {$UNDEF ASM_BP}
  {$DEFINE ASM_FPC}
  {$UNDEF BP_VMTLink}
{$ENDIF}

{---------------------------------------------------------------------------}
{  FPC LINUX COMPILER changes operating system - Updated 27Aug98 LdB        }
{  Note: Other linux compilers would need to change other details           }
{---------------------------------------------------------------------------}
{$IFDEF LINUX}
  {$UNDEF OS_DOS}
  {$DEFINE OS_LINUX}
{$ENDIF}

{---------------------------------------------------------------------------}
{  FPC GO32V2 COMPILER changes operating system - Updated 27Aug98 LdB       }
{---------------------------------------------------------------------------}
{$IFDEF GO32V2}
  {$DEFINE OS_GO32}
{$ENDIF}

{---------------------------------------------------------------------------}
{  32 BIT WINDOWS COMPILERS changes bit size - Updated 27Aug98 LdB          }
{---------------------------------------------------------------------------}
{$IFDEF WIN32}
  {$IFNDEF WINDOWS}
    {$DEFINE WINDOWS}
  {$ENDIF}
  {$UNDEF BIT_16}
  {$DEFINE BIT_32}
{$ENDIF}

{---------------------------------------------------------------------------}
{  WINDOWS COMPILERS change op system and proc mode - Updated 27Aug98 LdB   }
{---------------------------------------------------------------------------}
{$IFDEF WINDOWS}
  {$UNDEF OS_DOS}
  {$DEFINE OS_WINDOWS}
  {$UNDEF PROC_Real}
  {$DEFINE PROC_Protected}
{$ENDIF}

{---------------------------------------------------------------------------}
{  DELPHI1 COMPILER changes compiler type - Updated 27Aug98 LdB             }
{---------------------------------------------------------------------------}
{$IFDEF VER80}
  {$UNDEF PPC_BP}
  {$DEFINE PPC_DELPHI}
{$ENDIF}

{---------------------------------------------------------------------------}
{  DELPHI2 COMPILER changes compiler type - Updated 27Aug98 LdB             }
{---------------------------------------------------------------------------}
{$IFDEF VER90}
  {$UNDEF PPC_BP}
  {$DEFINE PPC_DELPHI}
  {$DEFINE PPC_DELPHI2}
{$ENDIF}

{---------------------------------------------------------------------------}
{  DELPHI3 COMPILER changes compiler type - Updated 27Aug98 LdB             }
{---------------------------------------------------------------------------}
{$IFDEF VER100}
  {$UNDEF PPC_BP}
  {$DEFINE PPC_DELPHI}
  {$DEFINE PPC_DELPHI3}
  {$UNDEF BP_VMTLink}
{$ENDIF}

{---------------------------------------------------------------------------}
{  DELPHI4 COMPILER changes compiler type - Updated 25Oct98 pfv             }
{---------------------------------------------------------------------------}
{$IFDEF VER120}
  {$UNDEF PPC_BP}
  {$DEFINE PPC_DELPHI}
  {$DEFINE PPC_DELPHI3}
  {$DEFINE PPC_DELPHI4}
  {$UNDEF BP_VMTLink}
{$ENDIF}

{---------------------------------------------------------------------------}
{  OS2 COMPILERS change compiler type and mode - Updated 27Aug98 LdB        }
{  Note: Assumes BPOS2 16BIT OS2 patch except for FPC which undefines this  }
{---------------------------------------------------------------------------}
{$IFDEF OS2}
  {$UNDEF OS_DOS}
  {$DEFINE OS_OS2}
  {$UNDEF PROC_Real}
  {$DEFINE PROC_Protected}
  {$UNDEF PPC_BP}
  {$DEFINE PPC_BPOS2}
  {$IFDEF FPC}
    {$UNDEF PPC_BPOS2}
  {$ENDIF}
{$ENDIF}

{---------------------------------------------------------------------------}
{  VIRTUAL PASCAL changes compiler type/32 bit - Updated 27Aug98 LdB        }
{  Note: VP2 can compile win 32 code so changes op system as needed         }
{---------------------------------------------------------------------------}
{$IFDEF VirtualPascal}
  {$UNDEF BIT_16}
  {$DEFINE BIT_32}
  {$IFDEF PPC_BPOS2}
    {$UNDEF PPC_BPOS2}
  {$ENDIF}
  {$DEFINE PPC_VIRTUAL}
  {$IFDEF WIN32}
    {$UNDEF PPC_BP}
    {$UNDEF OS_OS2}
    {$DEFINE OS_WINDOWS}
  {$ENDIF}
{$ENDIF}

{---------------------------------------------------------------------------}
{  SPEED COMPILER changes compiler type/32 bit  - Updated 27Aug98 LdB       }
{---------------------------------------------------------------------------}
{$IFDEF Speed}
  {$UNDEF BIT_16}
  {$DEFINE BIT_32}
  {$UNDEF PPC_BPOS2}
  {$DEFINE PPC_SPEED}
{$ENDIF}

{---------------------------------------------------------------------------}
{  FPC AMIGA COMPILER changes op system and CPU type - Updated 27Aug98 LdB  }
{---------------------------------------------------------------------------}
{$IFDEF AMIGA}
  {$UNDEF OS_DOS}
  {$DEFINE OS_AMIGA}
  {$IFDEF CPU86}
    {$UNDEF CPU86}
  {$ENDIF}
  {$IFNDEF CPU68}
    {$DEFINE CPU68}
  {$ENDIF}
{$ENDIF}

{---------------------------------------------------------------------------}
{  FPC ATARI COMPILER changes op system and CPU type - Updated 27Aug98 LdB  }
{---------------------------------------------------------------------------}
{$IFDEF ATARI}
  {$UNDEF OS_DOS}
  {$DEFINE OS_ATARI}
  {$IFDEF CPU86}
    {$UNDEF CPU86}
  {$ENDIF}
  {$IFNDEF CPU68}
    {$DEFINE CPU68}
  {$ENDIF}
{$ENDIF}

{---------------------------------------------------------------------------}
{  FPC MAC COMPILER changes op system and CPU type - Updated 27Aug98 LdB    }
{---------------------------------------------------------------------------}
{$IFDEF MACOS}
  {$UNDEF OS_DOS}
  {$DEFINE OS_MAC}
  {$IFDEF CPU86}
    {$UNDEF CPU86}
  {$ENDIF}
  {$IFNDEF CPU68}
    {$DEFINE CPU68}
  {$ENDIF}
{$ENDIF}

{
  $Log$
  Revision 1.1  2000-07-13 06:29:38  michael
  + Initial import

  Revision 1.1  2000/01/06 01:20:31  peter
    * moved out of packages/ back to topdir

  Revision 1.1  1999/12/23 19:36:47  peter
    * place unitfiles in target dirs

  Revision 1.1  1999/11/24 23:36:37  peter
    * moved to packages dir

  Revision 1.1  1998/12/04 12:48:24  peter
    * moved some dirs

  Revision 1.5  1998/10/26 11:22:53  peter
    * updates

}