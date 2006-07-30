Program TestList;

{$mode objfpc}{$h+}

Uses classes;

const a1 : pchar = '0';
      a2 : pchar = '1';
      a3 : pchar = '2';
      a4 : pchar = '3';
      a5 : pchar = '4';
      a6 : pchar = '5';
      a7 : pchar = '6';
      a8 : pchar = '7';
      a9 : pchar = '8';
      a10 : pchar = '9';

Var List : TList;
    StartMem,Runner : longint;

Function ACompare (P1,P2 : Pointer) : Integer;

Type PByte = ^Byte;

begin
  Result:=PByte(p1)^-PByte(P2)^;
end;

Procedure DumpMem;

begin
  Writeln ('    usedbytes : ',getfpcheapstatus.currheapused,' (=',getfpcheapstatus.currheapused-StartMem,' Bytes lost).')
end;

Procedure DumpList;

Var I : longint;

begin
  Write ('Count/Capacity : ',List.Count,'/',List.Capacity);dumpmem;
  If List.Count>0 then
    begin
    For i:=0 to List.Count-1 do
      if assigned(List.items[I]) then write (Pchar(List.items[i])) else write ('*');
    Writeln;
    end;
end;


begin
  StartMem:=getfpcheapstatus.currheapused;
  Writeln ('Creating List');
  List:=TList.Create;
  DumpList;
  Writeln ('Increasing capacity to 10');
  List.Capacity:=10;
  DumpList;
  Writeln ('Setting capacity to zero');
  List.capacity:=0;
  DumpList;
  Writeln ('Adding 10 elements in random sequence.');
  List.add (a2);
  List.add (a1);
  List.add (a3);
  List.add (a8);
  List.add (a5);
  List.add (a9);
  List.add (a4);
  List.Add (a8);
  List.Add (a7);
  List.Add (a6);
  Dumplist;
  Writeln ('Removing Third element.');
  List.Delete(2);
  DumpList;
  Writeln ('Inserting "0" at third place');
  List.Insert (2,a1);
  DumpList;
  Writeln ('Setting elmts 3 to 6 to Nil.');
  For Runner:=2 to 5 do List.Items[Runner]:=Nil;
  Dumplist;
  Writeln ('Packing list');
  List.Pack;
  DumpList;
  Writeln ('Setting capacity to count');
  List.Capacity:=List.Count;
  DumpList;
  Writeln ('Expanding list');
  List.Expand;
  DumpList;
  Writeln ('Index of ',a1,' : ',List.IndexOf(a1));
  Writeln ('Removing "',A1,'" from list.');
  List.Remove (a1);
  DumpList;
  Writeln ('Sorting List.');
  List.Sort (@ACompare);
  DumpList;
  Writeln ('Freeing list.');
  List.Free;
  DumpMem;
end.
