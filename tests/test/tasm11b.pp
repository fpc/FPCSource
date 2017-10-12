{ %CPU=i386 }
{ %NORUN }

{$asmmode att}

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
  end;
end.
