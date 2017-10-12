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
  end;
end.
