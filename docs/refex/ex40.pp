Program Example40;

{ Program to demonstrate the MaxAvail function. }

Var
  P : Pointer;
  I : longint;

begin
  { This will allocate memory until there is no more memory}
  I:=0;
  While MaxAvail>=1000 do
    begin
    Inc (I);
    GetMem (P,1000);
    end;
  { Default 4MB heap is allocated, so 4000 blocks
    should be allocated.
    When compiled with the -Ch10000 switch, the program
    will be able to allocate 10 block }
  Writeln ('Allocated ',i,' blocks of 1000 bytes');
end.
