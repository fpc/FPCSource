program bug0201;

{$asmmode att}

type rec = record
         a : DWord;
         b : Word;
     end;

procedure x(r1 : rec; r2 : rec; var r3 : rec);
begin
asm
   movl r3, %edi
   
   movl r1.a, %eax
   addl r2.a, %eax
   movl %eax, rec.a(%edi)

   movw r1.b, %cx
   addw r2.b, %cx
   movw %cx, rec.b(%edi)
end;
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

