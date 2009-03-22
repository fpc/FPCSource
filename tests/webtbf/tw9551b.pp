{ %fail }

uses 
  TypInfo,sysutils;

type
  tmy2enum = (e1=20,e2,e3,e4);


procedure Test;
begin
  writeln(GetEnumName(TypeInfo(TMy2Enum), Ord(e3)));
end;

begin 
 Test;
end.
