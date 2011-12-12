program tw20212;

{$mode objfpc}{$H+}

{$ifdef cpujvm}
uses
 {$ifdef java}jdk15{$else}androidr14{$endif};

{$macro on}
{$define writeln:=JLSystem.fout.println}
{$endif}


type
 generic TLinkedList<T> = class
 private
   FData: T;
   FNext: TLinkedList;
 public
   property Data: T read FData write FData;
   property Next: TLinkedList read FNext write FNext;
   constructor Create(const AData: T);
   procedure Append(const AData: T);
 end;

constructor TLinkedList.Create(const AData: T);
begin
 FData := AData;
 FNext := nil;
end;

procedure TLinkedList.Append(const AData: T);
var
  tmp: TLinkedList;
begin
 tmp:=FNext;
 while Assigned(tmp) and Assigned(tmp.Next) do
   tmp := tmp.Next;
 if Assigned(tmp) then
   tmp.Next := TLinkedList.Create(AData)
 else
   FNext := TLinkedList.Create(AData);
end;

type
 TIntegerLinkedList = specialize TLinkedList<Integer>;
var
 L, it: TIntegerLinkedList;
begin
 L := TIntegerLinkedList.Create(1);
 L.Append(1);
 L.Append(2);
 L.Append(3);
 L.Append(5);
 L.Append(8);
 L.Append(11);
 it:=l;
 while assigned(it) do
   begin
     writeln(it.data);
     it:=it.next;
   end;
end.

