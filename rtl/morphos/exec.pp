{
    $Id$
    This file is part of the Free Pascal run time library.
    Copyright (c) 2004 Karoly Balogh for Genesi S.a.r.l. <www.genesi.lu>

    exec.library interface unit for MorphOS/PowerPC

    MorphOS port was done on a free Pegasos II/G4 machine 
    provided by Genesi S.a.r.l. <www.genesi.lu>

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}

unit exec;

interface

var
  ExecBase: Pointer;

{$include execd.inc}
{$include execf.inc}


function NewGetTaskAttrs(Task    : PTask;
                         Data    : Pointer;
                         DataSize: Cardinal;
                         TType   : Cardinal;
                         Tags    : array of DWord): Cardinal; Inline;

implementation


function NewGetTaskAttrs(Task    : PTask;
                         Data    : Pointer;
                         DataSize: Cardinal;
                         TType   : Cardinal;
                         Tags    : array of DWord): Cardinal; Inline;
begin
  NewGetTaskAttrs:=NewGetTaskAttrsA(Task,Data,DataSize,TType,@Tags);
end;


begin
  ExecBase:=MOS_ExecBase;
end.

{
  $Log$
  Revision 1.3  2004-12-10 12:50:34  karoly
    * more ugly workarounds until compiler gets updated

  Revision 1.2  2004/08/03 15:35:23  karoly
    - removed conflicting calls

  Revision 1.1  2004/06/13 22:46:36  karoly
    * initial revision

}
