{ %CPU=x86_64 }
{ %NORUN }

{$asmmode intel}

begin
  asm
    { no params }
    movsb
    movsw
    movsd
    movsq

    cmpsb
    cmpsw
    cmpsd
    cmpsq

    scasb
    scasw
    scasd
    scasq

    lodsb
    lodsw
    lodsd
    lodsq

    stosb
    stosw
    stosd
    stosq

    insb
    insw
    insd

    outsb
    outsw
    outsd

    xlat
    xlatb

    { no segment overrides }
    xlat byte ptr [rbx]
    xlat byte ptr [ebx]

    movs byte ptr [rdi], byte ptr [rsi]
    movs byte ptr [edi], byte ptr [esi]
    movs word ptr [rdi], word ptr [rsi]
    movs word ptr [edi], word ptr [esi]
    movs dword ptr [rdi], dword ptr [rsi]
    movs dword ptr [edi], dword ptr [esi]
    movs qword ptr [rdi], qword ptr [rsi]
    movs qword ptr [edi], qword ptr [esi]

    cmps byte ptr [rsi], byte ptr [rdi]
    cmps byte ptr [esi], byte ptr [edi]
    cmps word ptr [rsi], word ptr [rdi]
    cmps word ptr [esi], word ptr [edi]
    cmps dword ptr [rsi], dword ptr [rdi]
    cmps dword ptr [esi], dword ptr [edi]
    cmps qword ptr [rsi], qword ptr [rdi]
    cmps qword ptr [esi], qword ptr [edi]

    scas byte ptr [rdi]
    scas byte ptr [edi]
    scas word ptr [rdi]
    scas word ptr [edi]
    scas dword ptr [rdi]
    scas dword ptr [edi]
    scas qword ptr [rdi]
    scas qword ptr [edi]

    lods byte ptr [rsi]
    lods byte ptr [esi]
    lods word ptr [rsi]
    lods word ptr [esi]
    lods dword ptr [rsi]
    lods dword ptr [esi]
    lods qword ptr [rsi]
    lods qword ptr [esi]

    stos byte ptr [rdi]
    stos byte ptr [edi]
    stos word ptr [rdi]
    stos word ptr [edi]
    stos dword ptr [rdi]
    stos dword ptr [edi]
    stos qword ptr [rdi]
    stos qword ptr [edi]

    ins byte ptr [rdi], dx
    ins byte ptr [edi], dx
    ins word ptr [rdi], dx
    ins word ptr [edi], dx
    ins dword ptr [rdi], dx
    ins dword ptr [edi], dx

    outs dx, byte ptr [rsi]
    outs dx, byte ptr [esi]
    outs dx, word ptr [rsi]
    outs dx, word ptr [esi]
    outs dx, dword ptr [rsi]
    outs dx, dword ptr [esi]

    { es:di }
    xlat byte ptr ds:[rbx]
    xlat byte ptr ds:[ebx]

    movs byte ptr es:[rdi], byte ptr [rsi]
    movs byte ptr es:[edi], byte ptr [esi]
    movs word ptr es:[rdi], word ptr [rsi]
    movs word ptr es:[edi], word ptr [esi]
    movs dword ptr es:[rdi], dword ptr [rsi]
    movs dword ptr es:[edi], dword ptr [esi]
    movs qword ptr es:[rdi], qword ptr [rsi]
    movs qword ptr es:[edi], qword ptr [esi]

    cmps byte ptr [rsi], byte ptr es:[rdi]
    cmps byte ptr [esi], byte ptr es:[edi]
    cmps word ptr [rsi], word ptr es:[rdi]
    cmps word ptr [esi], word ptr es:[edi]
    cmps dword ptr [rsi], dword ptr es:[rdi]
    cmps dword ptr [esi], dword ptr es:[edi]
    cmps qword ptr [rsi], qword ptr es:[rdi]
    cmps qword ptr [esi], qword ptr es:[edi]

    scas byte ptr es:[rdi]
    scas byte ptr es:[edi]
    scas word ptr es:[rdi]
    scas word ptr es:[edi]
    scas dword ptr es:[rdi]
    scas dword ptr es:[edi]
    scas qword ptr es:[rdi]
    scas qword ptr es:[edi]

    lods byte ptr [rsi]
    lods byte ptr [esi]
    lods word ptr [rsi]
    lods word ptr [esi]
    lods dword ptr [rsi]
    lods dword ptr [esi]
    lods qword ptr [rsi]
    lods qword ptr [esi]

    stos byte ptr es:[rdi]
    stos byte ptr es:[edi]
    stos word ptr es:[rdi]
    stos word ptr es:[edi]
    stos dword ptr es:[rdi]
    stos dword ptr es:[edi]
    stos qword ptr es:[rdi]
    stos qword ptr es:[edi]

    ins byte ptr es:[rdi], dx
    ins byte ptr es:[edi], dx
    ins word ptr es:[rdi], dx
    ins word ptr es:[edi], dx
    ins dword ptr es:[rdi], dx
    ins dword ptr es:[edi], dx

    outs dx, byte ptr [rsi]
    outs dx, byte ptr [esi]
    outs dx, word ptr [rsi]
    outs dx, word ptr [esi]
    outs dx, dword ptr [rsi]
    outs dx, dword ptr [esi]

    { es:di, fs:si }
    xlat byte ptr fs:[rbx]
    xlat byte ptr fs:[ebx]

    movs byte ptr es:[rdi], byte ptr fs:[rsi]
    movs byte ptr es:[edi], byte ptr fs:[esi]
    movs word ptr es:[rdi], word ptr fs:[rsi]
    movs word ptr es:[edi], word ptr fs:[esi]
    movs dword ptr es:[rdi], dword ptr fs:[rsi]
    movs dword ptr es:[edi], dword ptr fs:[esi]
    movs qword ptr es:[rdi], qword ptr fs:[rsi]
    movs qword ptr es:[edi], qword ptr fs:[esi]

    cmps byte ptr fs:[rsi], byte ptr es:[rdi]
    cmps byte ptr fs:[esi], byte ptr es:[edi]
    cmps word ptr fs:[rsi], word ptr es:[rdi]
    cmps word ptr fs:[esi], word ptr es:[edi]
    cmps dword ptr fs:[rsi], dword ptr es:[rdi]
    cmps dword ptr fs:[esi], dword ptr es:[edi]
    cmps qword ptr fs:[rsi], qword ptr es:[rdi]
    cmps qword ptr fs:[esi], qword ptr es:[edi]

    scas byte ptr es:[rdi]
    scas byte ptr es:[edi]
    scas word ptr es:[rdi]
    scas word ptr es:[edi]
    scas dword ptr es:[rdi]
    scas dword ptr es:[edi]
    scas qword ptr es:[rdi]
    scas qword ptr es:[edi]

    lods byte ptr fs:[rsi]
    lods byte ptr fs:[esi]
    lods word ptr fs:[rsi]
    lods word ptr fs:[esi]
    lods dword ptr fs:[rsi]
    lods dword ptr fs:[esi]
    lods qword ptr fs:[rsi]
    lods qword ptr fs:[esi]

    stos byte ptr es:[rdi]
    stos byte ptr es:[edi]
    stos word ptr es:[rdi]
    stos word ptr es:[edi]
    stos dword ptr es:[rdi]
    stos dword ptr es:[edi]
    stos qword ptr es:[rdi]
    stos qword ptr es:[edi]

    ins byte ptr es:[rdi], dx
    ins byte ptr es:[edi], dx
    ins word ptr es:[rdi], dx
    ins word ptr es:[edi], dx
    ins dword ptr es:[rdi], dx
    ins dword ptr es:[edi], dx

    outs dx, byte ptr fs:[rsi]
    outs dx, byte ptr fs:[esi]
    outs dx, word ptr fs:[rsi]
    outs dx, word ptr fs:[esi]
    outs dx, dword ptr fs:[rsi]
    outs dx, dword ptr fs:[esi]
  end;
end.
