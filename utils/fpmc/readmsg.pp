{
    $Id: readmsg.pp,v 1.2 2005/02/14 17:13:39 peter Exp $
    This file is part of the Free Pascal run time library.
    Copyright (c) 1999-2000 by the Free Pascal development team

    reads and dumps a message file to screen.

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}
{$mode objfpc}
{$h+}
program readmsg;

Type
  PCardinal = ^Cardinal;

Var
  F : File of Cardinal;
  PO,PI : PCardinal;
  I,J,Count,C,S : Cardinal;
  Buf : String;

begin
  Assign(F,Paramstr(1));
  Reset(F);
  Read(F,Count);
  Writeln('Message count: ',Count);
  S:=SizeOf(Cardinal)*Count+1;
  GetMem(PO,S);
  GetMem(PI,S);
  FillChar(PI^,S,0);
  FillChar(PO^,S,0);
  For I:=1 to Count do
    begin
    Read(F,C);
    PI[I]:=C;
    Read(F,C);
    If (C<>PI[I]) then
      Writeln('Error in ID: ',C,'<>ID',PI[I])
    else
      Writeln('Found ID ',C);
    Read(F,C);
    PO[I]:=C;
    Writeln('Found offset : ',C);
    end;
  For I:=1 to Count do
    begin
    Seek(F,PO[I] div 4);
    Read(F,S);
    Writeln('Found offset ',S,' at item ',i,' offset ',PO[I]);
    For J:=1 to (S div 4)-1 do
      begin
      Read(F,C);
      Move(C,Buf[J*4-3],4);
      end;
    J:=S-4;
    While Buf[J]=#0 do
      dec(J);
    SetLength(Buf,J);
    Writeln('String (',J,') : ',Buf);
    end;
  Writeln('Seqential read : ');
  Seek(F,PO[1] div 4);
  For I:=1 to Count do
    begin
    Read(F,S);
    Writeln('Found offset ',S,' at item ',i,' offset ',FilePos(F));
    For J:=1 to (S div 4)-1 do
      begin
      Read(F,C);
      Move(C,Buf[J*4-3],4);
      end;
    J:=S-4;
    While Buf[J]=#0 do
      dec(J);
    SetLength(Buf,J);
    Writeln('String (',J,') : ',Buf);
    end;
  Close(F);
end.

{
  $Log: readmsg.pp,v $
  Revision 1.2  2005/02/14 17:13:39  peter
    * truncate log

  Revision 1.1  2003/02/14 21:59:21  michael
  + Initial implementation

}