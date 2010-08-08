{ %fail }

uses 
  TypInfo,sysutils;

const
  csMyConstValueA = 10;
  csMyConstValueB = 20;
  csMyConstValueC = 30;

type
  TMyEnum = (meValueA = csMyConstValueA,
             meValueB = csMyConstValueB,
             meValueC = csMyConstValueC);

procedure Test;
begin
  writeln(GetEnumName(TypeInfo(TMyEnum), Ord(meValueB)));
end;

begin 
 Test;
end.
