{ %CPU=i386 }
{ Old file: tbs0034.pp }
{  shows wrong line numbering when asmbler is parsed in direct mode. }

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
