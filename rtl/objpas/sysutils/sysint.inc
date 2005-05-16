{
    $Id: sysint.inc,v 1.3 2005/03/13 14:30:24 marco Exp $
    This file is part of the Free Pascal run time library.
    Copyright (c) 2005 by the Free Pascal development team

    International settings for Sysutils unit.

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}


procedure InitInternationalGeneric;
  begin
    fillchar(SysLocale,sizeof(SysLocale),0);
    
    { keep these routines out of the executable? }
{$ifndef VER1_0}
{$ifndef FPC_NOGENERICANSIROUTINES}
    widestringmanager.UpperAnsiStringProc:=@GenericAnsiUpperCase;
    widestringmanager.LowerAnsiStringProc:=@GenericAnsiLowerCase;    
    widestringmanager.CompareStrAnsiStringProc:=@GenericAnsiCompareStr;
    widestringmanager.CompareTextAnsiStringProc:=@GenericAnsiCompareText;
    widestringmanager.StrCompAnsiStringProc:=@GenericAnsiStrComp;
    widestringmanager.StrICompAnsiStringProc:=@GenericAnsiStrIComp;
    widestringmanager.StrLCompAnsiStringProc:=@GenericAnsiStrLComp;
    widestringmanager.StrLICompAnsiStringProc:=@GenericAnsiStrLIComp;
    widestringmanager.StrLowerAnsiStringProc:=@GenericAnsiStrLower;
    widestringmanager.StrUpperAnsiStringProc:=@GenericAnsiStrUpper;
{$endif FPC_NOGENERICANSIROUTINES}    
{$endif}
  end;
  
{
  $Log: sysint.inc,v $
  Revision 1.3  2005/03/13 14:30:24  marco
   * some 1.0.x fixes

  Revision 1.2  2005/03/12 14:56:22  florian
    + added Ansi* routines to widestring manager
    * made them using OS calls on windows

  Revision 1.1  2005/02/26 19:25:01  florian
    + initial commit
}  