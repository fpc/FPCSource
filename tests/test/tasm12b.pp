{ %CPU=i386 }
{ %NORUN }

{$asmmode intel}

begin
  asm
    { no params }
    movsb
    movsw
    movsd

    cmpsb
    cmpsw
    cmpsd

    scasb
    scasw
    scasd

    lodsb
    lodsw
    lodsd

    stosb
    stosw
    stosd

    insb
    insw
    insd

    outsb
    outsw
    outsd

    xlat
    xlatb

    { no segment overrides }
    xlat byte ptr [ebx]
    xlat byte ptr [bx]

    movs byte ptr [edi], byte ptr [esi]
    movs byte ptr [di], byte ptr [si]
    movs word ptr [edi], word ptr [esi]
    movs word ptr [di], word ptr [si]
    movs dword ptr [edi], dword ptr [esi]
    movs dword ptr [di], dword ptr [si]

    cmps byte ptr [esi], byte ptr [edi]
    cmps byte ptr [si], byte ptr [di]
    cmps word ptr [esi], word ptr [edi]
    cmps word ptr [si], word ptr [di]
    cmps dword ptr [esi], dword ptr [edi]
    cmps dword ptr [si], dword ptr [di]

    scas byte ptr [edi]
    scas byte ptr [di]
    scas word ptr [edi]
    scas word ptr [di]
    scas dword ptr [edi]
    scas dword ptr [di]

    lods byte ptr [esi]
    lods byte ptr [si]
    lods word ptr [esi]
    lods word ptr [si]
    lods dword ptr [esi]
    lods dword ptr [si]

    stos byte ptr [edi]
    stos byte ptr [di]
    stos word ptr [edi]
    stos word ptr [di]
    stos dword ptr [edi]
    stos dword ptr [di]

    ins byte ptr [edi], dx
    ins byte ptr [di], dx
    ins word ptr [edi], dx
    ins word ptr [di], dx
    ins dword ptr [edi], dx
    ins dword ptr [di], dx

    outs dx, byte ptr [esi]
    outs dx, byte ptr [si]
    outs dx, word ptr [esi]
    outs dx, word ptr [si]
    outs dx, dword ptr [esi]
    outs dx, dword ptr [si]

    { es:di }
    xlat byte ptr ds:[ebx]
    xlat byte ptr ds:[bx]

    movs byte ptr es:[edi], byte ptr [esi]
    movs byte ptr es:[di], byte ptr [si]
    movs word ptr es:[edi], word ptr [esi]
    movs word ptr es:[di], word ptr [si]
    movs dword ptr es:[edi], dword ptr [esi]
    movs dword ptr es:[di], dword ptr [si]

    cmps byte ptr [esi], byte ptr es:[edi]
    cmps byte ptr [si], byte ptr es:[di]
    cmps word ptr [esi], word ptr es:[edi]
    cmps word ptr [si], word ptr es:[di]
    cmps dword ptr [esi], dword ptr es:[edi]
    cmps dword ptr [si], dword ptr es:[di]

    scas byte ptr es:[edi]
    scas byte ptr es:[di]
    scas word ptr es:[edi]
    scas word ptr es:[di]
    scas dword ptr es:[edi]
    scas dword ptr es:[di]

    lods byte ptr [esi]
    lods byte ptr [si]
    lods word ptr [esi]
    lods word ptr [si]
    lods dword ptr [esi]
    lods dword ptr [si]

    stos byte ptr es:[edi]
    stos byte ptr es:[di]
    stos word ptr es:[edi]
    stos word ptr es:[di]
    stos dword ptr es:[edi]
    stos dword ptr es:[di]

    ins byte ptr es:[edi], dx
    ins byte ptr es:[di], dx
    ins word ptr es:[edi], dx
    ins word ptr es:[di], dx
    ins dword ptr es:[edi], dx
    ins dword ptr es:[di], dx

    outs dx, byte ptr [esi]
    outs dx, byte ptr [si]
    outs dx, word ptr [esi]
    outs dx, word ptr [si]
    outs dx, dword ptr [esi]
    outs dx, dword ptr [si]

    { es:di, fs:si }
    xlat byte ptr fs:[ebx]
    xlat byte ptr fs:[bx]

    movs byte ptr es:[edi], byte ptr fs:[esi]
    movs byte ptr es:[di], byte ptr fs:[si]
    movs word ptr es:[edi], word ptr fs:[esi]
    movs word ptr es:[di], word ptr fs:[si]
    movs dword ptr es:[edi], dword ptr fs:[esi]
    movs dword ptr es:[di], dword ptr fs:[si]

    cmps byte ptr fs:[esi], byte ptr es:[edi]
    cmps byte ptr fs:[si], byte ptr es:[di]
    cmps word ptr fs:[esi], word ptr es:[edi]
    cmps word ptr fs:[si], word ptr es:[di]
    cmps dword ptr fs:[esi], dword ptr es:[edi]
    cmps dword ptr fs:[si], dword ptr es:[di]

    scas byte ptr es:[edi]
    scas byte ptr es:[di]
    scas word ptr es:[edi]
    scas word ptr es:[di]
    scas dword ptr es:[edi]
    scas dword ptr es:[di]

    lods byte ptr fs:[esi]
    lods byte ptr fs:[si]
    lods word ptr fs:[esi]
    lods word ptr fs:[si]
    lods dword ptr fs:[esi]
    lods dword ptr fs:[si]

    stos byte ptr es:[edi]
    stos byte ptr es:[di]
    stos word ptr es:[edi]
    stos word ptr es:[di]
    stos dword ptr es:[edi]
    stos dword ptr es:[di]

    ins byte ptr es:[edi], dx
    ins byte ptr es:[di], dx
    ins word ptr es:[edi], dx
    ins word ptr es:[di], dx
    ins dword ptr es:[edi], dx
    ins dword ptr es:[di], dx

    outs dx, byte ptr fs:[esi]
    outs dx, byte ptr fs:[si]
    outs dx, word ptr fs:[esi]
    outs dx, word ptr fs:[si]
    outs dx, dword ptr fs:[esi]
    outs dx, dword ptr fs:[si]
  end;
end.
