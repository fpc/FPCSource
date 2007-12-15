{ %CPU=i386 }
{ %OPT=-Cg- }
{ Old file: tbs0124b.pp }
{  }

{$asmmode intel}
var
 i : byte;
 l : array[0..7] of longint;
begin
 { problem here is that l is replaced by BP-offset     }
 { relative to stack, and the parser thinks all wrong  }
 { because of this.                                    }

 for i:=0 to 7 do
  l[i]:=35;
 asm
        mov     eax,3
        mov     byte ptr l[eax*4],55
 end;
 if l[3]<>55 then
   begin
      Writeln('Error in parsing assembler');
      Halt(1);
   end;
end.
