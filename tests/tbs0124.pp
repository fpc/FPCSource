{$I386_INTEL}
{ Compile with -Rintel switch }
var
 l : longint;
begin
 { problem here is that l is replaced by BP-offset     }
 { relative to stack, and the parser thinks all wrong  }
 { because of this.                                    }
 asm
   mov eax, [eax*4+l]    
 end;
end. 
