{
    $Id$
    Copyright (c) 1998-2000 by Michael Van Canneyt

    Unit to catch segmentation faults and Ctrl-C and exit gracefully
    under linux and go32v2

    This program is free software; you can redistribute it and/or modify
    it under the terms of the GNU General Public License as published by
    the Free Software Foundation; either version 2 of the License, or
    (at your option) any later version.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
    GNU General Public License for more details.

    You should have received a copy of the GNU General Public License
    along with this program; if not, write to the Free Software
    Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.

  *********************************************************************
}
Unit catch;

{$i fpcdefs.inc}

{$ifdef go32v2}
  { go32v2 stack check goes nuts if ss is not the data selector (PM) }
  {$S-}
{$endif}

{$ifdef DEBUG}
  {$define NOCATCH}
{$endif DEBUG}

interface
uses
{$ifdef unix}
  {$define has_signal}
  {$ifdef ver1_0}
    Linux,
  {$else}
    Unix,
  {$endif}
{$endif}
{$ifdef go32v2}
  {$define has_signal}
  dpmiexcp,
{$endif}
  verbose;

{$ifdef has_signal}
Var
  NewSignal,OldSigSegm,
  OldSigInt,OldSigFPE : SignalHandler;
{$endif}

Const in_const_evaluation : boolean = false;

Implementation

{$ifdef has_signal}
{$ifdef unix}
Procedure CatchSignal(Sig : SmallInt);cdecl;
{$else}
Function CatchSignal(Sig : longint):longint;
{$endif}
begin
  case Sig of
   SIGSEGV : begin
             { Temporary message - until we get an error number... }
               writeln ('Panic : Internal compiler error, exiting.');
               internalerror(9999);
             end;
    SIGFPE : begin
               If in_const_evaluation then
                 Writeln('FPE error computing constant expression')
               else
                 Writeln('FPE error inside compiler');
               Stop;
             end;
    SIGINT : begin
               WriteLn('Ctrl-C Signaled!');
               Stop;
             end;
  end;
{$ifndef unix}
  CatchSignal:=0;
{$endif}
end;
{$endif def has_signal}


begin
{$ifndef nocatch}
  {$ifdef has_signal}
    NewSignal:=SignalHandler({$ifdef fpcprocvar}@{$endif}CatchSignal);
    {$ifndef sunos}
      OldSigSegm:=Signal (SIGSEGV,NewSignal);
    {$endif} // lxrun on solaris hooks this for handling linux-calls!
    OldSigInt:=Signal (SIGINT,NewSignal);
    OldSigFPE:=Signal (SIGFPE,NewSignal);
  {$endif}
{$endif nocatch}
end.

{
  $Log$
  Revision 1.10  2002-05-16 19:46:35  carl
  + defines.inc -> fpcdefs.inc to avoid conflicts if compiling by hand
  + try to fix temp allocation (still in ifdef)
  + generic constructor calls
  + start of tassembler / tmodulebase class cleanup

  Revision 1.8  2001/02/26 19:44:52  peter
    * merged generic m68k updates from fixes branch

  Revision 1.7  2001/02/05 20:47:00  peter
    * support linux unit for ver1_0 compilers

  Revision 1.6  2001/01/21 20:32:45  marco
   * Renamefest. Compiler part. Not that hard.

  Revision 1.5  2000/11/13 15:26:12  marco
   * Renamefest

  Revision 1.4  2000/09/24 15:06:11  peter
    * use defines.inc

  Revision 1.3  2000/09/10 20:26:55  peter
    * bsd patches from marco

  Revision 1.2  2000/07/13 11:32:32  michael
  + removed logs
}
