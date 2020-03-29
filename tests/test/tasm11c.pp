{ %CPU=i8086 }
{ %NORUN }

{$asmmode att}
{$asmcpu 80386}

begin
  asm
    movsb
    movsw
    movsl

    cmpsb
    cmpsw
    cmpsl

    scasb
    scasw
    scasl

    lodsb
    lodsw
    lodsl

    stosb
    stosw
    stosl

    insb
    insw
    insl

    outsb
    outsw
    outsl

    { no segment overrides }
    movsb (%esi), (%edi)
    movsb (%si), (%di)
    movsw (%esi), (%edi)
    movsw (%si), (%di)
    movsl (%esi), (%edi)
    movsl (%si), (%di)

    cmpsb (%edi), (%esi)
    cmpsb (%di), (%si)
    cmpsw (%edi), (%esi)
    cmpsw (%di), (%si)
    cmpsl (%edi), (%esi)
    cmpsl (%di), (%si)

    scasb (%edi)
    scasb (%di)
    scasw (%edi)
    scasw (%di)
    scasl (%edi)
    scasl (%di)

    lodsb (%esi)
    lodsb (%si)
    lodsw (%esi)
    lodsw (%si)
    lodsl (%esi)
    lodsl (%si)

    stosb (%edi)
    stosb (%di)
    stosw (%edi)
    stosw (%di)
    stosl (%edi)
    stosl (%di)

    insb %dx,(%edi)
    insb %dx,(%di)
    insw %dx,(%edi)
    insw %dx,(%di)
    insl %dx,(%edi)
    insl %dx,(%di)

    outsb (%esi),%dx
    outsb (%si),%dx
    outsw (%esi),%dx
    outsw (%si),%dx
    outsl (%esi),%dx
    outsl (%si),%dx

    { es:di }
    movsb (%esi), %es:(%edi)
    movsb (%si), %es:(%di)
    movsw (%esi), %es:(%edi)
    movsw (%si), %es:(%di)
    movsl (%esi), %es:(%edi)
    movsl (%si), %es:(%di)

    cmpsb %es:(%edi), (%esi)
    cmpsb %es:(%di), (%si)
    cmpsw %es:(%edi), (%esi)
    cmpsw %es:(%di), (%si)
    cmpsl %es:(%edi), (%esi)
    cmpsl %es:(%di), (%si)

    scasb %es:(%edi)
    scasb %es:(%di)
    scasw %es:(%edi)
    scasw %es:(%di)
    scasl %es:(%edi)
    scasl %es:(%di)

    lodsb (%esi)
    lodsb (%si)
    lodsw (%esi)
    lodsw (%si)
    lodsl (%esi)
    lodsl (%si)

    stosb %es:(%edi)
    stosb %es:(%di)
    stosw %es:(%edi)
    stosw %es:(%di)
    stosl %es:(%edi)
    stosl %es:(%di)

    insb %dx,%es:(%edi)
    insb %dx,%es:(%di)
    insw %dx,%es:(%edi)
    insw %dx,%es:(%di)
    insl %dx,%es:(%edi)
    insl %dx,%es:(%di)

    outsb (%esi),%dx
    outsb (%si),%dx
    outsw (%esi),%dx
    outsw (%si),%dx
    outsl (%esi),%dx
    outsl (%si),%dx

    { es:di, fs:si }
    movsb %fs:(%esi), %es:(%edi)
    movsb %fs:(%si), %es:(%di)
    movsw %fs:(%esi), %es:(%edi)
    movsw %fs:(%si), %es:(%di)
    movsl %fs:(%esi), %es:(%edi)
    movsl %fs:(%si), %es:(%di)

    cmpsb %es:(%edi), %fs:(%esi)
    cmpsb %es:(%di), %fs:(%si)
    cmpsw %es:(%edi), %fs:(%esi)
    cmpsw %es:(%di), %fs:(%si)
    cmpsl %es:(%edi), %fs:(%esi)
    cmpsl %es:(%di), %fs:(%si)

    scasb %es:(%edi)
    scasb %es:(%di)
    scasw %es:(%edi)
    scasw %es:(%di)
    scasl %es:(%edi)
    scasl %es:(%di)

    lodsb %fs:(%esi)
    lodsb %fs:(%si)
    lodsw %fs:(%esi)
    lodsw %fs:(%si)
    lodsl %fs:(%esi)
    lodsl %fs:(%si)

    stosb %es:(%edi)
    stosb %es:(%di)
    stosw %es:(%edi)
    stosw %es:(%di)
    stosl %es:(%edi)
    stosl %es:(%di)

    insb %dx,%es:(%edi)
    insb %dx,%es:(%di)
    insw %dx,%es:(%edi)
    insw %dx,%es:(%di)
    insl %dx,%es:(%edi)
    insl %dx,%es:(%di)

    outsb %fs:(%esi),%dx
    outsb %fs:(%si),%dx
    outsw %fs:(%esi),%dx
    outsw %fs:(%si),%dx
    outsl %fs:(%esi),%dx
    outsl %fs:(%si),%dx
  end;
end.
