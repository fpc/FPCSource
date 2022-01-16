    _start:
        clr r1

{$ifdef RELBRANCHES}
        rjmp PASCALMAIN
{$else RELBRANCHES}
        jmp PASCALMAIN
{$endif RELBRANCHES}
        .text

