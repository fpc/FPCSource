{ %CPU=x86_64 }
{ %NORUN }

{$asmmode att}

begin
  asm
    movsb
    movsw
    movsl
    movsq

    cmpsb
    cmpsw
    cmpsl
    cmpsq

    scasb
    scasw
    scasl
    scasq

    lodsb
    lodsw
    lodsl
    lodsq

    stosb
    stosw
    stosl
    stosq

    insb
    insw
    insl

    outsb
    outsw
    outsl

    { no segment overrides }
    movsb (%rsi), (%rdi)
    movsb (%esi), (%edi)
    movsw (%rsi), (%rdi)
    movsw (%esi), (%edi)
    movsl (%rsi), (%rdi)
    movsl (%esi), (%edi)
    movsq (%rsi), (%rdi)
    movsq (%esi), (%edi)

    cmpsb (%rdi), (%rsi)
    cmpsb (%edi), (%esi)
    cmpsw (%rdi), (%rsi)
    cmpsw (%edi), (%esi)
    cmpsl (%rdi), (%rsi)
    cmpsl (%edi), (%esi)
    cmpsq (%rdi), (%rsi)
    cmpsq (%edi), (%esi)

    scasb (%rdi)
    scasb (%edi)
    scasw (%rdi)
    scasw (%edi)
    scasl (%rdi)
    scasl (%edi)
    scasq (%rdi)
    scasq (%edi)

    lodsb (%rsi)
    lodsb (%esi)
    lodsw (%rsi)
    lodsw (%esi)
    lodsl (%rsi)
    lodsl (%esi)
    lodsq (%rsi)
    lodsq (%esi)

    stosb (%rdi)
    stosb (%edi)
    stosw (%rdi)
    stosw (%edi)
    stosl (%rdi)
    stosl (%edi)
    stosq (%rdi)
    stosq (%edi)

    insb %dx,(%rdi)
    insb %dx,(%edi)
    insw %dx,(%rdi)
    insw %dx,(%edi)
    insl %dx,(%rdi)
    insl %dx,(%edi)

    outsb (%rsi),%dx
    outsb (%esi),%dx
    outsw (%rsi),%dx
    outsw (%esi),%dx
    outsl (%rsi),%dx
    outsl (%esi),%dx

    { es:di }
    movsb (%rsi), %es:(%rdi)
    movsb (%esi), %es:(%edi)
    movsw (%rsi), %es:(%rdi)
    movsw (%esi), %es:(%edi)
    movsl (%rsi), %es:(%rdi)
    movsl (%esi), %es:(%edi)
    movsq (%rsi), %es:(%rdi)
    movsq (%esi), %es:(%edi)

    cmpsb %es:(%rdi), (%rsi)
    cmpsb %es:(%edi), (%esi)
    cmpsw %es:(%rdi), (%rsi)
    cmpsw %es:(%edi), (%esi)
    cmpsl %es:(%rdi), (%rsi)
    cmpsl %es:(%edi), (%esi)
    cmpsq %es:(%rdi), (%rsi)
    cmpsq %es:(%edi), (%esi)

    scasb %es:(%rdi)
    scasb %es:(%edi)
    scasw %es:(%rdi)
    scasw %es:(%edi)
    scasl %es:(%rdi)
    scasl %es:(%edi)
    scasq %es:(%rdi)
    scasq %es:(%edi)

    lodsb (%rsi)
    lodsb (%esi)
    lodsw (%rsi)
    lodsw (%esi)
    lodsl (%rsi)
    lodsl (%esi)
    lodsq (%rsi)
    lodsq (%esi)

    stosb %es:(%rdi)
    stosb %es:(%edi)
    stosw %es:(%rdi)
    stosw %es:(%edi)
    stosl %es:(%rdi)
    stosl %es:(%edi)
    stosq %es:(%rdi)
    stosq %es:(%edi)

    insb %dx,%es:(%rdi)
    insb %dx,%es:(%edi)
    insw %dx,%es:(%rdi)
    insw %dx,%es:(%edi)
    insl %dx,%es:(%rdi)
    insl %dx,%es:(%edi)

    outsb (%rsi),%dx
    outsb (%esi),%dx
    outsw (%rsi),%dx
    outsw (%esi),%dx
    outsl (%rsi),%dx
    outsl (%esi),%dx

    { es:di, fs:si }
    movsb %fs:(%rsi), %es:(%rdi)
    movsb %fs:(%esi), %es:(%edi)
    movsw %fs:(%rsi), %es:(%rdi)
    movsw %fs:(%esi), %es:(%edi)
    movsl %fs:(%rsi), %es:(%rdi)
    movsl %fs:(%esi), %es:(%edi)
    movsq %fs:(%rsi), %es:(%rdi)
    movsq %fs:(%esi), %es:(%edi)

    cmpsb %es:(%rdi), %fs:(%rsi)
    cmpsb %es:(%edi), %fs:(%esi)
    cmpsw %es:(%rdi), %fs:(%rsi)
    cmpsw %es:(%edi), %fs:(%esi)
    cmpsl %es:(%rdi), %fs:(%rsi)
    cmpsl %es:(%edi), %fs:(%esi)
    cmpsq %es:(%rdi), %fs:(%rsi)
    cmpsq %es:(%edi), %fs:(%esi)

    scasb %es:(%rdi)
    scasb %es:(%edi)
    scasw %es:(%rdi)
    scasw %es:(%edi)
    scasl %es:(%rdi)
    scasl %es:(%edi)
    scasq %es:(%rdi)
    scasq %es:(%edi)

    lodsb %fs:(%rsi)
    lodsb %fs:(%esi)
    lodsw %fs:(%rsi)
    lodsw %fs:(%esi)
    lodsl %fs:(%rsi)
    lodsl %fs:(%esi)
    lodsq %fs:(%rsi)
    lodsq %fs:(%esi)

    stosb %es:(%rdi)
    stosb %es:(%edi)
    stosw %es:(%rdi)
    stosw %es:(%edi)
    stosl %es:(%rdi)
    stosl %es:(%edi)
    stosq %es:(%rdi)
    stosq %es:(%edi)

    insb %dx,%es:(%rdi)
    insb %dx,%es:(%edi)
    insw %dx,%es:(%rdi)
    insw %dx,%es:(%edi)
    insl %dx,%es:(%rdi)
    insl %dx,%es:(%edi)

    outsb %fs:(%rsi),%dx
    outsb %fs:(%esi),%dx
    outsw %fs:(%rsi),%dx
    outsw %fs:(%esi),%dx
    outsl %fs:(%rsi),%dx
    outsl %fs:(%esi),%dx
  end;
end.
