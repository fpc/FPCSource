{$asmmode intel}
type
  trec = record
    ypos,
    xpos : longint;
  end;
   
  z80cont = record
     page: array [0..11,0..16383] of byte;
  end;
  
var
  rec : tRec;
  myz80 : z80cont;
begin
  asm
     lea     edi, Rec
     cmp   byte ptr [edi+tRec.Xpos], true
     cmp   byte ptr [edi].trec.Xpos, true
     
     mov dh,byte ptr myz80.page[ecx]
  end;
end.
