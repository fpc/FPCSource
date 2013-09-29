{$i ../mips/mips.inc}

{$ifndef FPC_SYSTEM_HAS_SAR_QWORD}
{$ifdef ENDIAN_LITTLE}
{$define FPC_SYSTEM_HAS_SAR_QWORD}
function fpc_SarInt64(Const AValue : Int64;const Shift : Byte): Int64; [Public,Alias:'FPC_SARINT64']; compilerproc; assembler; nostackframe;
asm
{ $a1=high(AValue),$a0=low(AValue), result: $v1:$v0 }
    andi   $a2,$a2,63
    sltiu  $t0,$a2,32
    beq    $t0,$0,.L1
    nop

    srlv   $v0,$a0,$a2
    srav   $v1,$a1,$a2
    beq    $a2,$0,.Lexit
    nop
    subu   $t0,$0,$a2
    sllv   $t0,$a1,$t0
    or     $v0,$v0,$t0
    b      .Lexit
    nop
.L1:
    sra    $v1,$a1,31
    srav   $v0,$a1,$a2
.Lexit:
end;
{$endif ENDIAN_LITTLE}
{$endif FPC_SYSTEM_HAS_SAR_QWORD}

