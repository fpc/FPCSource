{
    $Id: sysutils.pp,v 1.59 2005/03/25 22:53:39 jonas Exp $
    This file is part of the Free Pascal run time library.
    Copyright (c) 1999-2000 by Florian Klaempfl
    member of the Free Pascal development team

    Sysutils unit for linux

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}


Const 
  KernelUUID       = '/proc/sys/kernel/random/uuid';


Procedure GetURandomBytes(Var Buf; NBytes : Integer);

Var
  fd,I : Integer;
  P : PByte;
  
begin
  P:=@Buf;
  fd:=FileOpen('/dev/urandom',fmOpenRead);
  if (fd>=0) then
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
    end
  else
    GetRandomBytes(Buf,NBytes);
end;


Function CreateKernelGUID(Var GUID : TGUID) : Boolean;

Const
  UUIDLen = 36;

Var
  fd: Longint;
  S : String;
  
begin
  fd:=FileOpen(KernelUUID,fmOpenRead);
  Result:=(Fd>=0);
  if Result then
    try
    SetLength(S,UUIDLen);
    SetLength(S,FileRead(fd,S[1],UUIDLen));
    Result:=(Length(S)=UUIDLen);
    If Result then
      GUID:=StringToGUID('{'+S+'}');
    finally
      FileClose(FD);  
    end;
    end;
end;

Function SysCreateGUID(out GUID : TGUID) : Integer;

begin
  if not CreateKernelGUID(Guid) then
    GetRandomBytes(GUID,SizeOf(Guid));  
  Result:=0;    
end;

