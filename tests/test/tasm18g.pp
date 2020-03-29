{ %CPU=i386 }
program tasm18g;

{$asmmode intel}
{$packrecords 1}

const
  cval = 1;

type
  foo = record
    b1: byte;
    w: word;
    b2: byte;
  end;
  foo2 = record
    bb1, bb2: byte;
  end;
  foo32 = record
    b1, b2, b3, b4: byte;
  end;
  foo32_2 = record
    b1: byte;
    l: longint;
  end;

const
  expect1: array [0..$CF] of byte = (
    $66,$F7,$07,$01,$00,      { TEST    WORD PTR [EDI],0001    }
    $F6,$07,$01,              { TEST    BYTE PTR [EDI],01      }
    $66,$F7,$47,$01,$01,$00,  { TEST    WORD PTR [EDI+01],0001 }
    $F6,$47,$03,$01,          { TEST    BYTE PTR [EDI+03],01   }
    $66,$F7,$47,$FF,$01,$00,  { TEST    WORD PTR [EDI-01],0001 }
    $F6,$47,$FD,$01,          { TEST    BYTE PTR [EDI-03],01   }
    $66,$F7,$47,$04,$01,$00,  { TEST    WORD PTR [EDI+04],0001 }
    $F6,$47,$04,$01,          { TEST    BYTE PTR [EDI+04],01   }

    $66,$F7,$07,$01,$00,      { TEST    WORD PTR [EDI],0001    }
    $F6,$07,$01,              { TEST    BYTE PTR [EDI],01      }
    $66,$F7,$47,$01,$01,$00,  { TEST    WORD PTR [EDI+01],0001 }
    $F6,$47,$03,$01,          { TEST    BYTE PTR [EDI+03],01   }
    $66,$F7,$47,$FF,$01,$00,  { TEST    WORD PTR [EDI-01],0001 }
    $F6,$47,$FD,$01,          { TEST    BYTE PTR [EDI-03],01   }
    $66,$F7,$47,$04,$01,$00,  { TEST    WORD PTR [EDI+04],0001 }
    $F6,$47,$04,$01,          { TEST    BYTE PTR [EDI+04],01   }

    $66,$F7,$07,$01,$00,      { TEST    WORD PTR [EDI],0001    }
    $F6,$07,$01,              { TEST    BYTE PTR [EDI],01      }
    $66,$F7,$47,$05,$01,$00,  { TEST    WORD PTR [EDI+05],0001 }
    $F6,$47,$0F,$01,          { TEST    BYTE PTR [EDI+0F],01   }
    $66,$F7,$47,$FB,$01,$00,  { TEST    WORD PTR [EDI-05],0001 }
    $F6,$47,$F1,$01,          { TEST    BYTE PTR [EDI-0F],01   }
    $66,$F7,$47,$12,$01,$00,  { TEST    WORD PTR [EDI+12],0001 }
    $F6,$47,$0E,$01,          { TEST    BYTE PTR [EDI+0E],01   }

    $F6,$07,$01,              { TEST    BYTE PTR [EDI],01      }
    $F6,$07,$01,              { TEST    BYTE PTR [EDI],01      }
    $F6,$47,$01,$01,          { TEST    BYTE PTR [EDI+01],01   }
    $F6,$47,$03,$01,          { TEST    BYTE PTR [EDI+03],01   }
    $F6,$47,$FF,$01,          { TEST    BYTE PTR [EDI-01],01   }
    $F6,$47,$FD,$01,          { TEST    BYTE PTR [EDI-03],01   }
    $F6,$47,$04,$01,          { TEST    BYTE PTR [EDI+04],01   }
    $F6,$47,$04,$01,          { TEST    BYTE PTR [EDI+04],01   }
    $F6,$07,$01,              { TEST    BYTE PTR [EDI],01      }
    $F6,$47,$01,$01,          { TEST    BYTE PTR [EDI+01],01   }

    $66,$F7,$07,$01,$00,      { TEST    WORD PTR [EDI],0001    }
    $66,$F7,$07,$01,$00,      { TEST    WORD PTR [EDI],0001    }
    $66,$F7,$47,$01,$01,$00,  { TEST    WORD PTR [EDI+01],0001 }
    $66,$F7,$47,$03,$01,$00,  { TEST    WORD PTR [EDI+03],0001 }
    $66,$F7,$47,$FF,$01,$00,  { TEST    WORD PTR [EDI-01],0001 }
    $66,$F7,$47,$FD,$01,$00,  { TEST    WORD PTR [EDI-03],0001 }
    $66,$F7,$47,$04,$01,$00,  { TEST    WORD PTR [EDI+04],0001 }
    $66,$F7,$47,$04,$01,$00,  { TEST    WORD PTR [EDI+04],0001 }
    $66,$F7,$07,$01,$00,      { TEST    WORD PTR [EDI],0001    }
    $66,$F7,$47,$01,$01,$00   { TEST    WORD PTR [EDI+01],0001 }
  );
  expect2: array [0..$42] of byte = (
    $F7,$07,$01,$00,$00,$00,      { TEST    DWORD PTR [EDI],00000001    }
    $F7,$47,$01,$01,$00,$00,$00,  { TEST    DWORD PTR [EDI+01],00000001 }

    $F7,$07,$01,$00,$00,$00,      { TEST    DWORD PTR [EDI],00000001    }
    $F7,$07,$01,$00,$00,$00,      { TEST    DWORD PTR [EDI],00000001    }
    $F7,$47,$01,$01,$00,$00,$00,  { TEST    DWORD PTR [EDI+01],00000001 }
    $F7,$47,$03,$01,$00,$00,$00,  { TEST    DWORD PTR [EDI+03],00000001 }
    $F7,$47,$FF,$01,$00,$00,$00,  { TEST    DWORD PTR [EDI-01],00000001 }
    $F7,$47,$FD,$01,$00,$00,$00,  { TEST    DWORD PTR [EDI-03],00000001 }
    $F7,$47,$04,$01,$00,$00,$00,  { TEST    DWORD PTR [EDI+04],00000001 }
    $F7,$47,$04,$01,$00,$00,$00   { TEST    DWORD PTR [EDI+04],00000001 }
  );


procedure test1; nostackframe; assembler;
asm
  test [edi+foo2], cval                   { test word ptr [edi], 1   }
  test byte ptr [edi+foo], cval           { test byte ptr [edi], 1   }
  test [edi+foo.w], cval                  { test word ptr [edi+1], 1 }
  test [edi+foo.b2], cval                 { test byte ptr [edi+3], 1 }
  test [edi-foo.w], cval                  { test word ptr [edi-1], 1 }
  test [edi-foo.b2], cval                 { test byte ptr [edi-3], 1 }
  test [edi+foo.b2+foo.w], cval           { test word ptr [edi+4], 1 }
  test [edi+foo.w+foo.b2], cval           { test byte ptr [edi+4], 1 }

  test [edi+foo2*1], cval                 { test word ptr [edi], 1   }
  test byte ptr [edi+foo*1], cval         { test byte ptr [edi], 1   }
  test [edi+foo.w*1], cval                { test word ptr [edi+1], 1 }
  test [edi+foo.b2*1], cval               { test byte ptr [edi+3], 1 }
  test [edi-foo.w*1], cval                { test word ptr [edi-1], 1 }
  test [edi-foo.b2*1], cval               { test byte ptr [edi-3], 1 }
  test [edi+foo.b2*1+foo.w*1], cval       { test word ptr [edi+4], 1 }
  test [edi+foo.w*1+foo.b2*1], cval       { test byte ptr [edi+4], 1 }

  test [edi+foo2*5], cval                 { test word ptr [edi], 1    }
  test byte ptr [edi+foo*5], cval         { test byte ptr [edi], 1    }
  test [edi+foo.w*5], cval                { test word ptr [edi+5], 1  }
  test [edi+foo.b2*5], cval               { test byte ptr [edi+15], 1 }
  test [edi-foo.w*5], cval                { test word ptr [edi-5], 1  }
  test [edi-foo.b2*5], cval               { test byte ptr [edi-15], 1 }
  test [edi+foo.b2*5+foo.w*3], cval       { test word ptr [edi+18], 1 }
  test [edi+foo.w*5+foo.b2*3], cval       { test byte ptr [edi+14], 1 }

  test byte ptr [edi+foo2], cval          { test byte ptr [edi], 1   }
  test byte ptr [edi+foo], cval           { test byte ptr [edi], 1   }
  test byte ptr [edi+foo.w], cval         { test byte ptr [edi+1], 1 }
  test byte ptr [edi+foo.b2], cval        { test byte ptr [edi+3], 1 }
  test byte ptr [edi-foo.w], cval         { test byte ptr [edi-1], 1 }
  test byte ptr [edi-foo.b2], cval        { test byte ptr [edi-3], 1 }
  test byte ptr [edi+foo.b2+foo.w], cval  { test byte ptr [edi+4], 1 }
  test byte ptr [edi+foo.w+foo.b2], cval  { test byte ptr [edi+4], 1 }
  test byte ptr [edi+foo32], cval         { test byte ptr [edi], 1   }
  test byte ptr [edi+foo32_2.l], cval     { test byte ptr [edi+1], 1 }

  test word ptr [edi+foo2], cval          { test word ptr [edi], 1   }
  test word ptr [edi+foo], cval           { test word ptr [edi], 1   }
  test word ptr [edi+foo.w], cval         { test word ptr [edi+1], 1 }
  test word ptr [edi+foo.b2], cval        { test word ptr [edi+3], 1 }
  test word ptr [edi-foo.w], cval         { test word ptr [edi-1], 1 }
  test word ptr [edi-foo.b2], cval        { test word ptr [edi-3], 1 }
  test word ptr [edi+foo.b2+foo.w], cval  { test word ptr [edi+4], 1 }
  test word ptr [edi+foo.w+foo.b2], cval  { test word ptr [edi+4], 1 }
  test word ptr [edi+foo32], cval         { test word ptr [edi], 1   }
  test word ptr [edi+foo32_2.l], cval     { test word ptr [edi+1], 1 }
end;

procedure test2; nostackframe; assembler;
asm
  test [edi+foo32], cval                   { test dword ptr [edi], 1   }
  test [edi+foo32_2.l], cval               { test dword ptr [edi+1], 1 }

  test dword ptr [edi+foo2], cval          { test dword ptr [edi], 1   }
  test dword ptr [edi+foo], cval           { test dword ptr [edi], 1   }
  test dword ptr [edi+foo.w], cval         { test dword ptr [edi+1], 1 }
  test dword ptr [edi+foo.b2], cval        { test dword ptr [edi+3], 1 }
  test dword ptr [edi-foo.w], cval         { test dword ptr [edi-1], 1 }
  test dword ptr [edi-foo.b2], cval        { test dword ptr [edi-3], 1 }
  test dword ptr [edi+foo.b2+foo.w], cval  { test dword ptr [edi+4], 1 }
  test dword ptr [edi+foo.w+foo.b2], cval  { test dword ptr [edi+4], 1 }
end;

procedure Error;
begin
  Writeln('Error!');
  Halt(1);
end;

function CompareCode(cp: CodePointer; dp: Pointer; sz: SizeInt): Boolean;
begin
  CompareCode := CompareByte(cp^, dp^, sz) = 0;
end;

begin
  if not CompareCode(CodePointer(@test1), @expect1, SizeOf(expect1)) then
    Error;
  if not CompareCode(CodePointer(@test2), @expect2, SizeOf(expect2)) then
    Error;

  Writeln('Ok!');
end.
