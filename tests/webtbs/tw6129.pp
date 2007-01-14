{$mode tp}
program objarray;

type

TAny = object
public
  constructor Init;
end;

TNameIndex = object(TAny)
  Name : ^string;
  Index: integer;
end;

constructor TAny.Init;
begin
end;

const

S0: string[5]='Obj-0';
S1: string[5]='Obj-1';
S2: string[5]='Obj-2';

Table: array[0..2] of TNameIndex = (
  (Name:@S0; Index:0),
  (Name:@S1; Index:1),
  (Name:@S2; Index:2)
);

begin
  WriteLn('@S0            = ',longint(@S0));
  WriteLn('@S1            = ',longint(@S1));
  WriteLn('@S2            = ',longint(@S2));

  WriteLn('Table[0].Name  = ',longint(Table[0].Name));
  WriteLn('Table[1].Name  = ',longint(unaligned(Table[1].Name)));
  WriteLn('Table[2].Name  = ',longint(Table[2].Name));

  WriteLn('Table[0].Name^ = ',Table[0].Name^);
  WriteLn('Table[1].Name^ = ',unaligned(Table[1].Name)^);
  WriteLn('Table[2].Name^ = ',Table[2].Name^);
end.