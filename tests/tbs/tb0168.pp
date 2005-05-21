{ %CPU=i386 }
{ %OPT= -Ratt }

{ Old file: tbs0201.pp }
{ problem with record var-parameters and assembler      OK 0.99.11 (PFV) }

program bug0201;

type rec = record
         a : DWord;
         b : Word;
     end;

{ this is really for tests but
  this should be coded with const r1 and r2 !! }

function x(r1 : rec; r2 : rec; var r3 : rec) : integer; assembler; {$ifndef ver1_0}oldfpccall;{$endif}
asm
   movl r3, %edi
   movl r1, %ebx
   movl r2, %ecx
   movl rec.a(%ebx), %eax
   addl rec.a(%ecx), %eax
   movl %eax, rec.a(%edi)

   movw rec.b(%ebx), %ax
   addw rec.b(%ecx), %ax
   movw %ax, rec.b(%edi)
   movw $1,%ax
end;

var r1, r2, r3 : rec;

begin
     r1.a := 100; r1.b := 200;
     r2.a := 300; r2.b := 400;
     x(r1, r2, r3);
     Writeln(r3.a, ' ', r3.b);
     if (r3.a<>400) or (r3.b<>600) then
       begin
          Writeln('Error in assembler code');
          Halt(1);
       end;
end.
