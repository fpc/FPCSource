program testuid;

uses sysutils, macuuid;

Var
  T : TGUID;
  P : PByte;
  I : Integer;

begin
//  GetURandomBytes(T,SizeOf(T));
  CreateGUID(T);
  Writeln(GUIDToString(T));
end.
