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
  Revision 1.4  2000-02-09 16:59:28  peter
    * truncated log

  Revision 1.3  2000/01/07 16:41:30  daniel
    * copyright 2000

}
