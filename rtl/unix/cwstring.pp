{
    $Id$
    This file is part of the Free Pascal run time library.
    Copyright (c) 2005 by Florian Klaempfl,
    member of the Free Pascal development team.

    libc based wide string support

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}
{$mode objfpc}

unit cwstring;
interface

 {$linklib c}		// try adding -Xf

Procedure SetCWidestringManager;

implementation

Uses
  systhrds,
  BaseUnix,
  unix,
  unixtype;
  
{ maybe this needs to be splitted in an os depend way later }
function towlower(__wc:wint_t):wint_t;cdecl;external clib name 'towlower';
function towupper(__wc:wint_t):wint_t;cdecl;external clib name 'towupper';  
  
function LowerWideCase(const s : WideString) : WideString;
  begin
  end;
  
  
function UpperWideCase(const s : WideString) : WideString;
  begin
  end;
  
  
function CompareWideString(const s1, s2 : WideString) : PtrInt;
  begin
  end;
  
  
function SameWideString(const s1, s2 : WideString) : Boolean;
  begin
  end;
  

Var
  CWideStringManager : TWideStringManager;

Procedure SetCWideStringManager;

begin
  With CWideStringManager do
    begin
    end;
  SetWideStringManager(CWideStringManager);
  InitHeapMutexes;
end;


initialization
  SetCWideStringManager;
finalization
end.
{
  $Log$
  Revision 1.1  2005-02-01 20:22:50  florian
    * improved widestring infrastructure manager
}

