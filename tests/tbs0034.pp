{ line numbering problem }
{ I don't really know how to test this (PM }
 var i : longint;

begin
   asm
      movl %eax,%eax
      movl %eax,%eax
      movl %eax,%eax
      movl %eax,%eax
      movl %eax,%eax
      movl %eax,%eax
      movl %eax,%eax
   end ;
   i:=0;
end.
