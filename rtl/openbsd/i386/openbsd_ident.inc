{$asmmode att}
{$if FPC_FULLVERSION>30300}
  {$define USE_NOTE}
{$endif}

procedure OpenBSDIdentTag;nostackframe;assembler;
  asm
    .section ".note.openbsd.ident", "a"{$ifdef USE_NOTE}, @note {$endif}
    .p2align 2
    .long    8
    .long    4
    .long    1
    .asciz   "OpenBSD"
    .long    0

    .text
  end;
