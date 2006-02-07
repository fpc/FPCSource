{
    $Id: sysutils.pp,v 1.59 2005/03/25 22:53:39 jonas Exp $
    This file is part of the Free Pascal run time library.
    Copyright (c) 2006 by Florian Klaempfl
    member of the Free Pascal development team

    uuidgen for Solaris

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}
Const
  RandomDevice  = '/dev/urandom';


Function GetURandomBytes(Var Buf; NBytes : Integer) : Boolean;

Var
  fd,I : Integer;
  P : PByte;

begin
  P:=@Buf;
  fd:=FileOpen(RandomDevice,fmOpenRead);
  Result:=(fd>=0);
  if Result then
    Try
      While (NBytes>0) do
        begin
        I:=FileRead(fd,P^,nbytes);
        If I>0 then
          begin
          Inc(P,I);
          Dec(NBytes,I);
          end;
        end;
    Finally
      FileClose(Fd);
    end;
end;


Function SysCreateGUID(out GUID : TGUID) : Integer;

begin
  if not GetUrandomBytes(Guid,SizeOf(GUID)) then
    GetRandomBytes(GUID,SizeOf(Guid));
  Result:=0;
end;
