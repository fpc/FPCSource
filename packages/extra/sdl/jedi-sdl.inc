{
  $Id: jedi-sdl.inc,v 1.9 2004/12/23 23:42:17 savage Exp $
}
{******************************************************************************}
{                                                                              }
{       Borland Delphi SDL - Simple DirectMedia Layer                          }
{       Global Conditional Definitions for JEDI-SDL cross-compilation          }
{                                                                              }
{                                                                              }
{ The initial developer of this Pascal code was :                              }
{  Prof. Abimbola Olowofoyeku <http://www.bigfoot.com/~African_Chief/>         }
{                                                                              }
{ Portions created by Prof. Abimbola Olowofoyeku are                           }
{ Copyright (C) 2000 - 2100 Prof. Abimbola Olowofoyeku.                        }
{                                                                              }
{                                                                              }
{ Contributor(s)                                                               }
{ --------------                                                               }
{  Prof. Abimbola Olowofoyeku <http://www.bigfoot.com/~African_Chief/>         }
{  Dominqiue Louis <Dominique@SavageSoftware.com.au>                           }
{                                                                              }
{ Obtained through:                                                            }
{ Joint Endeavour of Delphi Innovators ( Project JEDI )                        }
{                                                                              }
{ You may retrieve the latest version of this file at the Project              }
{ JEDI home page, located at http://delphi-jedi.org                            }
{                                                                              }
{ The contents of this file are used with permission, subject to               }
{ the Mozilla Public License Version 1.1 (the "License"); you may              }
{ not use this file except in compliance with the License. You may             }
{ obtain a copy of the License at                                              }
{ http://www.mozilla.org/MPL/MPL-1.1.html                                      }
{                                                                              }
{ Software distributed under the License is distributed on an                  }
{ "AS IS" basis, WITHOUT WARRANTY OF ANY KIND, either express or               }
{ implied. See the License for the specific language governing                 }
{ rights and limitations under the License.                                    }
{                                                                              }
{ Description                                                                  }
{ -----------                                                                  }
{ This code has been copied from...                                            }
{ Global Conditional Definitions for Chief's UNZIP package                     }
{ By Prof. Abimbola Olowofoyeku (The African Chief)                            }
{          http://www.bigfoot.com/~African_Chief/                              }
{                                                                              }
{                                                                              }
{ Requires                                                                     }
{ --------                                                                     }
{   The SDL Runtime libraris on Win32  : SDL.dll on Linux : libSDL.so          }
{   They are available from...                                                 }
{   http://www.libsdl.org .                                                    }
{                                                                              }
{ Programming Notes                                                            }
{ -----------------                                                            }
{                                                                              }
{                                                                              }
{                                                                              }
{                                                                              }
{ Revision History                                                             }
{ ----------------                                                             }
{   2003-04-03   DL - Initial addition                                         }
{                                                                              }
{   2003-04-07   DL - Added Macro ON derective for FPC and OpenGL and removed  }
{                WEAKPACKAGE derective. WEAKPACKAGE should be set when         }
{                appropriate.                                                  }
{                                                                              }
{   2003-04-23 - DL : under instruction from Alexey Barkovoy I have added      }
{                          better TMT Pascal support and under instruction     }
{                          from Prof. Abimbola Olowofoyeku (The African Chief) }
{                          I have added better Gnu Pascal support              }
{                                                                              }
{   2004-01-19 - DL : Under instruction from Marco van de Voort, I have added  }
{                     Better FPC support for FreeBSD.                          }
{                                                                              }
(*
  $Log: jedi-sdl.inc,v $
  Revision 1.9  2004/12/23 23:42:17  savage
  Applied Patches supplied by Michalis Kamburelis ( THANKS! ), for greater FreePascal compatability.

  Revision 1.8  2004/10/20 22:43:04  savage
  Ensure that UNSAFE type warning are off in D9 as well

  Revision 1.7  2004/04/05 09:59:51  savage
  Changes for FreePacal as suggested by Marco

  Revision 1.6  2004/03/31 22:18:15  savage
  Small comment for turning off warning under GnuPascal

  Revision 1.5  2004/03/30 22:41:02  savage
  Added extra commenting due to previous compiler directive

  Revision 1.4  2004/03/30 22:08:33  savage
  Added Kylix Define

  Revision 1.3  2004/03/30 21:34:40  savage
  {$H+} needed for FPC compatiblity

  Revision 1.2  2004/02/14 00:23:39  savage
  As UNIX is defined in jedi-sdl.inc this will be used to check linux compatability as well. Units have been changed to reflect this change.

*)
{******************************************************************************}

{.$define Debug}           { uncomment for debugging }

  {$MODE Delphi}          { use Delphi compatibility mode }
  {$H+}
  {$PACKRECORDS C}        // Added for record
  {$MACRO ON}             // Added For OpenGL
  {$DEFINE Delphi}
  {$DEFINE UseAT}
  {$UNDEF USE_STDCALL}
  {$DEFINE OS_BigMem}
  {$DEFINE NO_EXPORTS}
  {$DEFINE Has_Int64}
  {$DEFINE NOCRT}
  {$IFDEF unix}
     {$DEFINE fpc_unix}
  {$ELSE}
     {$DEFINE __OS_DOS__}
  {$ENDIF}
  {$IFDEF windows}
   {$DEFINE UseWin}
  {$ENDIF}

 {$IFDEF OS2}
    {$UNDEF Windows}
    {$DEFINE UseWin}
    {$DEFINE OS_BigMem}
 {$ENDIF OS2}

{$IFDEF windows}
  {$DEFINE OS_BigMem}
{$ELSE windows}
  {$IFDEF ver70}
    {$DEFINE assembler}
  {$ENDIF} { use 16-bit assembler! }
{$ENDIF windows}

{ ************************** dos/dos-like platforms **************}
{$IFDEF Windows}
   {$DEFINE __OS_DOS__}
   {$DEFINE UseWin}
   {$DEFINE MSWINDOWS}
{$ENDIF Delphi}

{$IFDEF OS2}
   {$DEFINE __OS_DOS__}
   {$DEFINE Can_Use_DLL}
{$ENDIF Delphi}

{$IFDEF UseWin}
   {$DEFINE Can_Use_DLL}
{$ENDIF}

{$IFDEF Win16}
   {$DEFINE Can_Use_DLL}
{$ENDIF}

{$IFDEF BP_DPMI}
   {$DEFINE Can_Use_DLL}
{$ENDIF}

{$IFDEF USE_STDCALL}
   {$IFNDEF __TMT__}
     {$DEFINE BY_NAME}
   {$ENDIF}
{$ENDIF}
