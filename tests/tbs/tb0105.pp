{ %TARGET=linux,go32v2 }
{ %CPU=i386 }
{ %OPT= -Aas }

{ Old file: tbs0124.pp }
{ Asm, problem with -Rintel switch and indexing         OK 0.99.11 (PM/PFV) }

{ this problem comes from the fact that
  L is a static variable, not a local one !!
  but the static variable symtable is the localst of the
  main procedure (PM)
  It must be checked if we are at main level or not !! }

var
 l : longint;

  procedure error;
    begin
       Writeln('Error in tbs0124');
       Halt(1);
    end;

begin
  l:=5;
{$asmmode att}
 asm
   movl  l,%eax
   addl  $2,%eax
   movl  %eax,l
 end;
  if l<>7 then error;
{$asmmode intel}
 { problem here is that l is replaced by BP-offset     }
 { relative to stack, and the parser thinks all wrong  }
 { because of this.                                    }
 asm
   mov eax,l
   add eax,5
   mov l,eax
 end;
 if l<>12 then error;
 Writeln('tbs0124 OK');
end.
