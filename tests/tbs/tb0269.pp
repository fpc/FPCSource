{ %CPU=i386}

{ Old file: tbs0313.pp }
{  }

  {$asmmode intel}
  TYPE
    TPoint3 = RECORD
      x,y,z:Single;
    END;

  OPERATOR + (CONST p1,p2:TPoint3) p : TPoint3; Assembler;
   ASM
     mov EBX,[p1]
     mov EDI,[p2]
     mov EDX,[p]
     movq MM0,[EBX]
     pfadd MM0,[EDI]
     movq [EDX],MM0
    { Now the correct way would be something like: }
     movd MM0,[EBX+8]  // [movd reg??,mem?? - invalid combination of opcod
     movd MM1,[EDI+8]  // and here, too
     pfadd MM0,MM1
     movd [EDX+8],MM0  // and here
     femms
   END;

begin
end.
