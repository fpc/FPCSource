{$asmmode intel}
var
 l : longint;
begin
 { problem here is that l is replaced by BP-offset     }
 { relative to stack, and the parser thinks all wrong  }
 { because of this.                                    }
 asm
        lea     eax,[eax*4+eax]
        mov     eax,[eax*4+l]
 end;
end.
