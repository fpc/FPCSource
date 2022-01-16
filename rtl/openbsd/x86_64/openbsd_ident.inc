{$asmmode gas}

procedure OpenBSDIdentTag;nostackframe;assembler;
  asm
    .section ".note.openbsd.ident", "a"
    .p2align 2
    .long    8
    .long    4
    .long    1
    .asciz   "OpenBSD"
    .long    0

    .text
  end;
