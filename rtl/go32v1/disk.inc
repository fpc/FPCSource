{
    $Id$
    This file is part of the Free Pascal run time library.
    Copyright (c) 1999-2000 by the Free Pascal development team

    Disk functions from Delphi's sysutils.pas

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}

Function DiskFree (Drive : Byte) : Longint;
var
  Regs: Registers;
begin
  Regs.Dl := Drive;
  Regs.Ah := $36;
  intr($21, Regs);
  if Regs.Ax <> $FFFF then
    result := Regs.Ax * Regs.Bx * Regs.Cx
  else
    result := -1;
end;


Function DiskSize (Drive : Byte) : Longint;
var
  Regs: Registers;
begin
  Regs.Dl := Drive;
  Regs.Ah := $36;
  Intr($21, Regs);
  if Regs.Ax <> $FFFF then
    result := Regs.Ax * Regs.Cx * Regs.Dx
  else
    result := -1;
end;


Function GetCurrentDir : String;
begin
  GetDir(0, result);
end;


Function SetCurrentDir (Const NewDir : String) : Boolean;
begin
  {$I-}
  ChDir(NewDir);
  result := (IOResult = 0);
  {$I+}
end;


Function CreateDir (Const NewDir : String) : Boolean;
begin
  {$I-}
  MkDir(NewDir);
  result := (IOResult = 0);
  {$I+}
end;


Function RemoveDir (Const Dir : String) : Boolean;
begin
  {$I-}
  RmDir(Dir);
  result := (IOResult = 0);
  {$I+}
end;


{
  $Log$
  Revision 1.1  2000-07-13 06:30:34  michael
  + Initial import

  Revision 1.3  2000/01/07 16:41:29  daniel
    * copyright 2000

  Revision 1.2  1999/04/08 12:22:53  peter
    * removed os.inc

  Revision 1.1  1998/12/21 13:07:02  peter
    * use -FE

  Revision 1.2  1998/10/30 14:13:13  michael
  + Implementation of functions by Gertjan Schouten

  Revision 1.1  1998/10/11 13:42:55  michael
  Added disk functions
}
