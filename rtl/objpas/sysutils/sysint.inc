{
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
  end;
  
