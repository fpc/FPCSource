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
