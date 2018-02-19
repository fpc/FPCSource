{ %CPU=avr}
{ %norun }

{$goto on}

{ Valid syntax that fail to assemble/link - a code gen issue?
  ldd r20, z+0
  std y+0, r20

 Instructions not implemented, seems to be XMEGA specific?:
 des, lac, las, lat, xch
}

program tbs614;

var a: byte;

label l1, l2;

procedure checklocalparam; assembler;
var
  avar: byte;
asm
  ldd r22, avar
end;

begin
  asm
    adc r0, r1
    add r31, r0
    adiw r24, 0
    adiw xl, 1
    adiw r30, 63
    and r0, r31
    andi r16, 0
    andi r16, 255
    asr r1
    bclr 0
    bclr 7
    bld r0, 7
    // Only test one of the branch variations, they all follow the same pattern
    l1:
    breq 2       // bug 32109
    breq l1
    break
    bset 0
    bset 7
    bst r0, 0
    bst r1, 7
    call l1
    {$ifdef CPUAVR_2_BYTE_PC}
    call 655034  // problematic due to current TOprRec.val size
    {$else}
    call 4194300  // problematic due to current TOprRec.val size
    {$endif}
    cbi 0x0, 0
    cbi 31, 7
    cbr r16, 0
    cbr R31, 255
    clc
    clh
    cli
    cln
    clr r1
    cls
    clt
    clv
    clz
    com r0
    com r31
    cp r0, r31
    cp r31, r0
    cp xl, xh
    cpc r0, r31
    cpi r16, 255
    cpi r31, 0
    cpse r0, r31
    dec r1
    {$ifdef CPUAVR_3_BYTE_PC}
    eicall
    eijmp
    {$endif}
    elpm
    elpm R30, z
    elpm R1,z+
    eor r0, r31
    fmul r16, r23
    fmuls r23, r16
    fmulsu r16, r23
    icall
    ijmp
    in R20, 0
    in R0, 63
    inc r0
    {$ifdef CPUAVR_HAS_JMP_CALL}
    jmp l1
    jmp 655034  // problematic due to current TOprRec.val size
    {$endif}
    //lac z, r0   // LAC, LAS, LAT not implemented in FPC, XMEGA only?
    ld r22, -x
    ld r22, x
    ld r22, x+
    ld r22, -y
    ld r22, y
    ld r22, y+
    ld r22, -y
    ld r22, y
    ld r22, y+
    ld r22, -z
    ld r22, z
    ld r22, z+
    ldd R20, y+6
    ldd R20, z+62
    ldi r16, 255
    ldi r31, 0
    lds R0, a
    lds R20, 100
    lds r0, 65530   // LDS have 2 versions, this is the 32 bit version limit which isn't available on all subarch's
    lpm
    lpm R30, z
    lpm R1,z+
    lsl r0
    lsr r0
    mov R0, R31
    mov xh, zl
    {$ifdef CPUAVR_HAS_MOVW}
    movw R0, R30
    movw XL, R2
    {$endif}
    mul r2, r31
    muls r16, r23
    mulsu r16, r23
    neg r0
    neg r31
    nop
    or r0, r31
    ori r16, 0
    ori r31, 255
    out 0, r1
    out 63, r1
    pop r0
    push r0
    rcall -2048  // problem with unsigned TOprRec.val: word
    rcall 2046
    rcall l2
    ret
    reti
    l2:
    rjmp -2048  // problem with unsigned TOprRec.val: word
    rjmp 2046
    rjmp l2
    rol r0
    ror r0
    sbc r31, r0
    sbci r16, 0
    sbci r31, 255
    sbi 0x0, 0
    sbi 31, 7
    sbic 15, 5
    sbis 15, 5
    {$ifdef CPUAVR_HAS_MOVW}
    sbiw r24, 63
    sbiw zl, 0
    {$endif}
    sbr r16, 255
    sbrc r0, 7
    sbrs r31, 0
    sec
    seh
    sei
    sen
    ser r16
    ser r31
    ses
    set
    sev
    sez
    sleep
    spm
    spm z
    spm z+
    st x, R0
    st -X, R0
    st X+, R0
    st y, R0
    st -y, R0
    st y+, r0
    st z, r0
    st -z, r0
    st z+, r0
    std y+2, R0
    std z+2, r20
    std z+1, r20
    sts a, r31
    sts 0, r0
    sts 65535, r31    // STS have 2 versions, this is the 32 bit version limit which isn't available on all subarch's
    sub r0, r31
    subi r16, 255
    swap r20
    tst r20
    wdr
  end;
end.
