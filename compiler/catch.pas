{
    $Id$
    Copyright (c) 1998-2002 by Michael Van Canneyt

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
{$ifdef watcom} // wiktor: pewnei nie potrzeba
  {$S-}
{$endif}

{$ifdef DEBUG}
  {$define NOCATCH}
{$endif DEBUG}

interface
uses
{$ifdef unix}
 {$ifndef beos}
  {$define has_signal}
  {$ifdef ver1_0}
    Linux,
  {$else}
    BaseUnix,Unix,
  {$endif}
 {$endif}
{$endif}
{$ifdef go32v2}
{$define has_signal}
  dpmiexcp,
{$endif}
{$ifdef watcom}
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
Procedure CatchSignal(Sig : Longint);cdecl;
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
      OldSigSegm:={$ifdef VER1_0}Signal{$else}fpSignal{$endif} (SIGSEGV,NewSignal);
    {$endif} // lxrun on solaris hooks this for handling linux-calls!
    OldSigInt:={$ifdef VER1_0}Signal{$else}fpSignal{$endif}  (SIGINT,NewSignal);
    OldSigFPE:={$ifdef VER1_0}Signal{$else}fpSignal{$endif}  (SIGFPE,NewSignal);
  {$endif}
{$endif nocatch}
end.

{
  $Log$
  Revision 1.15  2003-09-14 20:26:18  marco
   * Unix reform

  Revision 1.14  2003/09/05 17:41:12  florian
    * merged Wiktor's Watcom patches in 1.1

  Revision 1.13  2003/04/23 10:10:31  peter
    * unix signalhandler has longint argument

  Revision 1.12  2003/01/10 21:37:48  marco
   * beos shouldnt define hassignal (unix<-> hasunix problem)

  Revision 1.11  2002/05/18 13:34:05  peter
    * readded missing revisions

  Revision 1.10  2002/05/16 19:46:35  carl
  + defines.inc -> fpcdefs.inc to avoid conflicts if compiling by hand
  + try to fix temp allocation (still in ifdef)
  + generic constructor calls
  + start of tassembler / tmodulebase class cleanup

}
