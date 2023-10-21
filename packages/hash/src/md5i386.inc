// i386 assembler optimized version
procedure MD5Transform(var Context: TMDContext; Buffer: Pointer); assembler; nostackframe;
// eax = Context, edx = Buffer
{$asmmode intel}
asm
    push EBX
    push ESI
    push EDI
    push EAX // save Context

    // EBX = A, ECX = B, ESI = C, EDI = D
    mov EBX, TMDContext.State[EAX + 4*0] // A, B, C, D := Context.State[0 .. 3];
    mov ECX, TMDContext.State[EAX + 4*1]
    mov ESI, TMDContext.State[EAX + 4*2]
    mov EDI, TMDContext.State[EAX + 4*3] // From now on, EAX is used as a temporary.
// Round 1
//EBX := ECX + roldword(dword(EBX + ((ECX and ESI) or ((not ECX) and EDI)) + Data[0] + $d76aa478),  7);
    mov EAX, ESI
    add EBX, $d76aa478
    xor EAX, EDI
    add EBX, [EDX + 4*0]
    and EAX, ECX
    xor EAX, EDI
    add EBX, EAX
    rol EBX,  7
    add EBX, ECX

//EDI := EBX + roldword(dword(EDI + ((EBX and ECX) or ((not EBX) and ESI)) + Data[1] + $e8c7b756), 12);
    mov EAX, ECX
    add EDI, $e8c7b756
    xor EAX, ESI
    add EDI, [EDX + 4*1]
    and EAX, EBX
    xor EAX, ESI
    add EDI, EAX
    rol EDI, 12
    add EDI, EBX

//ESI := EDI + roldword(dword(ESI + ((EDI and EBX) or ((not EDI) and ECX)) + Data[2] + $242070db), 17);
    mov EAX, EBX
    add ESI, $242070db
    xor EAX, ECX
    add ESI, [EDX + 4*2]
    and EAX, EDI
    xor EAX, ECX
    add ESI, EAX
    rol ESI, 17
    add ESI, EDI

//ECX := ESI + roldword(dword(ECX + ((ESI and EDI) or ((not ESI) and EBX)) + Data[3] + $c1bdceee), 22);
    mov EAX, EDI
    add ECX, $c1bdceee
    xor EAX, EBX
    add ECX, [EDX + 4*3]
    and EAX, ESI
    xor EAX, EBX
    add ECX, EAX
    rol ECX, 22
    add ECX, ESI

//EBX := ECX + roldword(dword(EBX + ((ECX and ESI) or ((not ECX) and EDI)) + Data[4] + $f57c0faf),  7);
    mov EAX, ESI
    add EBX, $f57c0faf
    xor EAX, EDI
    add EBX, [EDX + 4*4]
    and EAX, ECX
    xor EAX, EDI
    add EBX, EAX
    rol EBX,  7
    add EBX, ECX

//EDI := EBX + roldword(dword(EDI + ((EBX and ECX) or ((not EBX) and ESI)) + Data[5] + $4787c62a), 12);
    mov EAX, ECX
    add EDI, $4787c62a
    xor EAX, ESI
    add EDI, [EDX + 4*5]
    and EAX, EBX
    xor EAX, ESI
    add EDI, EAX
    rol EDI, 12
    add EDI, EBX

//ESI := EDI + roldword(dword(ESI + ((EDI and EBX) or ((not EDI) and ECX)) + Data[6] + $a8304613), 17);
    mov EAX, EBX
    add ESI, $a8304613
    xor EAX, ECX
    add ESI, [EDX + 4*6]
    and EAX, EDI
    xor EAX, ECX
    add ESI, EAX
    rol ESI, 17
    add ESI, EDI

//ECX := ESI + roldword(dword(ECX + ((ESI and EDI) or ((not ESI) and EBX)) + Data[7] + $fd469501), 22);
    mov EAX, EDI
    add ECX, $fd469501
    xor EAX, EBX
    add ECX, [EDX + 4*7]
    and EAX, ESI
    xor EAX, EBX
    add ECX, EAX
    rol ECX, 22
    add ECX, ESI

//EBX := ECX + roldword(dword(EBX + ((ECX and ESI) or ((not ECX) and EDI)) + Data[8] + $698098d8),  7);
    mov EAX, ESI
    add EBX, $698098d8
    xor EAX, EDI
    add EBX, [EDX + 4*8]
    and EAX, ECX
    xor EAX, EDI
    add EBX, EAX
    rol EBX,  7
    add EBX, ECX

//EDI := EBX + roldword(dword(EDI + ((EBX and ECX) or ((not EBX) and ESI)) + Data[9] + $8b44f7af), 12);
    mov EAX, ECX
    add EDI, $8b44f7af
    xor EAX, ESI
    add EDI, [EDX + 4*9]
    and EAX, EBX
    xor EAX, ESI
    add EDI, EAX
    rol EDI, 12
    add EDI, EBX

//ESI := EDI + roldword(dword(ESI + ((EDI and EBX) or ((not EDI) and ECX)) + Data[10] + $ffff5bb1), 17);
    mov EAX, EBX
    add ESI, $ffff5bb1
    xor EAX, ECX
    add ESI, [EDX + 4*10]
    and EAX, EDI
    xor EAX, ECX
    add ESI, EAX
    rol ESI, 17
    add ESI, EDI

//ECX := ESI + roldword(dword(ECX + ((ESI and EDI) or ((not ESI) and EBX)) + Data[11] + $895cd7be), 22);
    mov EAX, EDI
    add ECX, $895cd7be
    xor EAX, EBX
    add ECX, [EDX + 4*11]
    and EAX, ESI
    xor EAX, EBX
    add ECX, EAX
    rol ECX, 22
    add ECX, ESI

//EBX := ECX + roldword(dword(EBX + ((ECX and ESI) or ((not ECX) and EDI)) + Data[12] + $6b901122),  7);
    mov EAX, ESI
    add EBX, $6b901122
    xor EAX, EDI
    add EBX, [EDX + 4*12]
    and EAX, ECX
    xor EAX, EDI
    add EBX, EAX
    rol EBX,  7
    add EBX, ECX

//EDI := EBX + roldword(dword(EDI + ((EBX and ECX) or ((not EBX) and ESI)) + Data[13] + $fd987193), 12);
    mov EAX, ECX
    add EDI, $fd987193
    xor EAX, ESI
    add EDI, [EDX + 4*13]
    and EAX, EBX
    xor EAX, ESI
    add EDI, EAX
    rol EDI, 12
    add EDI, EBX

//ESI := EDI + roldword(dword(ESI + ((EDI and EBX) or ((not EDI) and ECX)) + Data[14] + $a679438e), 17);
    mov EAX, EBX
    add ESI, $a679438e
    xor EAX, ECX
    add ESI, [EDX + 4*14]
    and EAX, EDI
    xor EAX, ECX
    add ESI, EAX
    rol ESI, 17
    add ESI, EDI

//ECX := ESI + roldword(dword(ECX + ((ESI and EDI) or ((not ESI) and EBX)) + Data[15] + $49b40821), 22);
    mov EAX, EDI
    add ECX, $49b40821
    xor EAX, EBX
    add ECX, [EDX + 4*15]
    and EAX, ESI
    xor EAX, EBX
    add ECX, EAX
    rol ECX, 22
    add ECX, ESI

// Round 2
//EBX := ECX + roldword(dword(EBX + ((ECX and EDI) or (ESI and (not EDI))) + Data[1] + $f61e2562),  5);
    mov EAX, ECX
    add EBX, $f61e2562
    xor EAX, ESI
    add EBX, [EDX + 4*1]
    and EAX, EDI
    xor EAX, ESI
    add EBX, EAX
    rol EBX,  5
    add EBX, ECX

//EDI := EBX + roldword(dword(EDI + ((EBX and ESI) or (ECX and (not ESI))) + Data[6] + $c040b340),  9);
    mov EAX, EBX
    add EDI, $c040b340
    xor EAX, ECX
    add EDI, [EDX + 4*6]
    and EAX, ESI
    xor EAX, ECX
    add EDI, EAX
    rol EDI,  9
    add EDI, EBX

//ESI := EDI + roldword(dword(ESI + ((EDI and ECX) or (EBX and (not ECX))) + Data[11] + $265e5a51), 14);
    mov EAX, EDI
    add ESI, $265e5a51
    xor EAX, EBX
    add ESI, [EDX + 4*11]
    and EAX, ECX
    xor EAX, EBX
    add ESI, EAX
    rol ESI, 14
    add ESI, EDI

//ECX := ESI + roldword(dword(ECX + ((ESI and EBX) or (EDI and (not EBX))) + Data[0] + $e9b6c7aa), 20);
    mov EAX, ESI
    add ECX, $e9b6c7aa
    xor EAX, EDI
    add ECX, [EDX + 4*0]
    and EAX, EBX
    xor EAX, EDI
    add ECX, EAX
    rol ECX, 20
    add ECX, ESI

//EBX := ECX + roldword(dword(EBX + ((ECX and EDI) or (ESI and (not EDI))) + Data[5] + $d62f105d),  5);
    mov EAX, ECX
    add EBX, $d62f105d
    xor EAX, ESI
    add EBX, [EDX + 4*5]
    and EAX, EDI
    xor EAX, ESI
    add EBX, EAX
    rol EBX,  5
    add EBX, ECX

//EDI := EBX + roldword(dword(EDI + ((EBX and ESI) or (ECX and (not ESI))) + Data[10] + $02441453),  9);
    mov EAX, EBX
    add EDI, $02441453
    xor EAX, ECX
    add EDI, [EDX + 4*10]
    and EAX, ESI
    xor EAX, ECX
    add EDI, EAX
    rol EDI,  9
    add EDI, EBX

//ESI := EDI + roldword(dword(ESI + ((EDI and ECX) or (EBX and (not ECX))) + Data[15] + $d8a1e681), 14);
    mov EAX, EDI
    add ESI, $d8a1e681
    xor EAX, EBX
    add ESI, [EDX + 4*15]
    and EAX, ECX
    xor EAX, EBX
    add ESI, EAX
    rol ESI, 14
    add ESI, EDI

//ECX := ESI + roldword(dword(ECX + ((ESI and EBX) or (EDI and (not EBX))) + Data[4] + $e7d3fbc8), 20);
    mov EAX, ESI
    add ECX, $e7d3fbc8
    xor EAX, EDI
    add ECX, [EDX + 4*4]
    and EAX, EBX
    xor EAX, EDI
    add ECX, EAX
    rol ECX, 20
    add ECX, ESI

//EBX := ECX + roldword(dword(EBX + ((ECX and EDI) or (ESI and (not EDI))) + Data[9] + $21e1cde6),  5);
    mov EAX, ECX
    add EBX, $21e1cde6
    xor EAX, ESI
    add EBX, [EDX + 4*9]
    and EAX, EDI
    xor EAX, ESI
    add EBX, EAX
    rol EBX,  5
    add EBX, ECX

//EDI := EBX + roldword(dword(EDI + ((EBX and ESI) or (ECX and (not ESI))) + Data[14] + $c33707d6),  9);
    mov EAX, EBX
    add EDI, $c33707d6
    xor EAX, ECX
    add EDI, [EDX + 4*14]
    and EAX, ESI
    xor EAX, ECX
    add EDI, EAX
    rol EDI,  9
    add EDI, EBX

//ESI := EDI + roldword(dword(ESI + ((EDI and ECX) or (EBX and (not ECX))) + Data[3] + $f4d50d87), 14);
    mov EAX, EDI
    add ESI, $f4d50d87
    xor EAX, EBX
    add ESI, [EDX + 4*3]
    and EAX, ECX
    xor EAX, EBX
    add ESI, EAX
    rol ESI, 14
    add ESI, EDI

//ECX := ESI + roldword(dword(ECX + ((ESI and EBX) or (EDI and (not EBX))) + Data[8] + $455a14ed), 20);
    mov EAX, ESI
    add ECX, $455a14ed
    xor EAX, EDI
    add ECX, [EDX + 4*8]
    and EAX, EBX
    xor EAX, EDI
    add ECX, EAX
    rol ECX, 20
    add ECX, ESI

//EBX := ECX + roldword(dword(EBX + ((ECX and EDI) or (ESI and (not EDI))) + Data[13] + $a9e3e905),  5);
    mov EAX, ECX
    add EBX, $a9e3e905
    xor EAX, ESI
    add EBX, [EDX + 4*13]
    and EAX, EDI
    xor EAX, ESI
    add EBX, EAX
    rol EBX,  5
    add EBX, ECX

//EDI := EBX + roldword(dword(EDI + ((EBX and ESI) or (ECX and (not ESI))) + Data[2] + $fcefa3f8),  9);
    mov EAX, EBX
    add EDI, $fcefa3f8
    xor EAX, ECX
    add EDI, [EDX + 4*2]
    and EAX, ESI
    xor EAX, ECX
    add EDI, EAX
    rol EDI,  9
    add EDI, EBX

//ESI := EDI + roldword(dword(ESI + ((EDI and ECX) or (EBX and (not ECX))) + Data[7] + $676f02d9), 14);
    mov EAX, EDI
    add ESI, $676f02d9
    xor EAX, EBX
    add ESI, [EDX + 4*7]
    and EAX, ECX
    xor EAX, EBX
    add ESI, EAX
    rol ESI, 14
    add ESI, EDI

//ECX := ESI + roldword(dword(ECX + ((ESI and EBX) or (EDI and (not EBX))) + Data[12] + $8d2a4c8a), 20);
    mov EAX, ESI
    add ECX, $8d2a4c8a
    xor EAX, EDI
    add ECX, [EDX + 4*12]
    and EAX, EBX
    xor EAX, EDI
    add ECX, EAX
    rol ECX, 20
    add ECX, ESI

// Round 3
//EBX := ECX + roldword(dword(EBX + (ECX xor ESI xor EDI) + Data[5] + $fffa3942),  4);
    mov EAX, ECX
    add EBX, $fffa3942
    xor EAX, ESI
    add EBX, [EDX + 4*5]
    xor EAX, EDI
    add EBX, EAX
    rol EBX,  4
    add EBX, ECX

//EDI := EBX + roldword(dword(EDI + (EBX xor ECX xor ESI) + Data[8] + $8771f681), 11);
    mov EAX, EBX
    add EDI, $8771f681
    xor EAX, ECX
    add EDI, [EDX + 4*8]
    xor EAX, ESI
    add EDI, EAX
    rol EDI, 11
    add EDI, EBX

//ESI := EDI + roldword(dword(ESI + (EDI xor EBX xor ECX) + Data[11] + $6d9d6122), 16);
    mov EAX, EDI
    add ESI, $6d9d6122
    xor EAX, EBX
    add ESI, [EDX + 4*11]
    xor EAX, ECX
    add ESI, EAX
    rol ESI, 16
    add ESI, EDI

//ECX := ESI + roldword(dword(ECX + (ESI xor EDI xor EBX) + Data[14] + $fde5380c), 23);
    mov EAX, ESI
    add ECX, $fde5380c
    xor EAX, EDI
    add ECX, [EDX + 4*14]
    xor EAX, EBX
    add ECX, EAX
    rol ECX, 23
    add ECX, ESI

//EBX := ECX + roldword(dword(EBX + (ECX xor ESI xor EDI) + Data[1] + $a4beea44),  4);
    mov EAX, ECX
    add EBX, $a4beea44
    xor EAX, ESI
    add EBX, [EDX + 4*1]
    xor EAX, EDI
    add EBX, EAX
    rol EBX,  4
    add EBX, ECX

//EDI := EBX + roldword(dword(EDI + (EBX xor ECX xor ESI) + Data[4] + $4bdecfa9), 11);
    mov EAX, EBX
    add EDI, $4bdecfa9
    xor EAX, ECX
    add EDI, [EDX + 4*4]
    xor EAX, ESI
    add EDI, EAX
    rol EDI, 11
    add EDI, EBX

//ESI := EDI + roldword(dword(ESI + (EDI xor EBX xor ECX) + Data[7] + $f6bb4b60), 16);
    mov EAX, EDI
    add ESI, $f6bb4b60
    xor EAX, EBX
    add ESI, [EDX + 4*7]
    xor EAX, ECX
    add ESI, EAX
    rol ESI, 16
    add ESI, EDI

//ECX := ESI + roldword(dword(ECX + (ESI xor EDI xor EBX) + Data[10] + $bebfbc70), 23);
    mov EAX, ESI
    add ECX, $bebfbc70
    xor EAX, EDI
    add ECX, [EDX + 4*10]
    xor EAX, EBX
    add ECX, EAX
    rol ECX, 23
    add ECX, ESI

//EBX := ECX + roldword(dword(EBX + (ECX xor ESI xor EDI) + Data[13] + $289b7ec6),  4);
    mov EAX, ECX
    add EBX, $289b7ec6
    xor EAX, ESI
    add EBX, [EDX + 4*13]
    xor EAX, EDI
    add EBX, EAX
    rol EBX,  4
    add EBX, ECX

//EDI := EBX + roldword(dword(EDI + (EBX xor ECX xor ESI) + Data[0] + $eaa127fa), 11);
    mov EAX, EBX
    add EDI, $eaa127fa
    xor EAX, ECX
    add EDI, [EDX + 4*0]
    xor EAX, ESI
    add EDI, EAX
    rol EDI, 11
    add EDI, EBX

//ESI := EDI + roldword(dword(ESI + (EDI xor EBX xor ECX) + Data[3] + $d4ef3085), 16);
    mov EAX, EDI
    add ESI, $d4ef3085
    xor EAX, EBX
    add ESI, [EDX + 4*3]
    xor EAX, ECX
    add ESI, EAX
    rol ESI, 16
    add ESI, EDI

//ECX := ESI + roldword(dword(ECX + (ESI xor EDI xor EBX) + Data[6] + $04881d05), 23);
    mov EAX, ESI
    add ECX, $04881d05
    xor EAX, EDI
    add ECX, [EDX + 4*6]
    xor EAX, EBX
    add ECX, EAX
    rol ECX, 23
    add ECX, ESI

//EBX := ECX + roldword(dword(EBX + (ECX xor ESI xor EDI) + Data[9] + $d9d4d039),  4);
    mov EAX, ECX
    add EBX, $d9d4d039
    xor EAX, ESI
    add EBX, [EDX + 4*9]
    xor EAX, EDI
    add EBX, EAX
    rol EBX,  4
    add EBX, ECX

//EDI := EBX + roldword(dword(EDI + (EBX xor ECX xor ESI) + Data[12] + $e6db99e5), 11);
    mov EAX, EBX
    add EDI, $e6db99e5
    xor EAX, ECX
    add EDI, [EDX + 4*12]
    xor EAX, ESI
    add EDI, EAX
    rol EDI, 11
    add EDI, EBX

//ESI := EDI + roldword(dword(ESI + (EDI xor EBX xor ECX) + Data[15] + $1fa27cf8), 16);
    mov EAX, EDI
    add ESI, $1fa27cf8
    xor EAX, EBX
    add ESI, [EDX + 4*15]
    xor EAX, ECX
    add ESI, EAX
    rol ESI, 16
    add ESI, EDI

//ECX := ESI + roldword(dword(ECX + (ESI xor EDI xor EBX) + Data[2] + $c4ac5665), 23);
    mov EAX, ESI
    add ECX, $c4ac5665
    xor EAX, EDI
    add ECX, [EDX + 4*2]
    xor EAX, EBX
    add ECX, EAX
    rol ECX, 23
    add ECX, ESI

// Round 4
//EBX := ECX + roldword(dword(EBX + (ESI xor (ECX or (not EDI))) + Data[0] + $f4292244),  6);
    mov EAX, EDI
    add EBX, $f4292244
    not EAX
    add EBX, [EDX + 4*0]
    or EAX, ECX
    xor EAX, ESI
    add EBX, EAX
    rol EBX,  6
    add EBX, ECX

//EDI := EBX + roldword(dword(EDI + (ECX xor (EBX or (not ESI))) + Data[7] + $432aff97), 10);
    mov EAX, ESI
    add EDI, $432aff97
    not EAX
    add EDI, [EDX + 4*7]
    or EAX, EBX
    xor EAX, ECX
    add EDI, EAX
    rol EDI, 10
    add EDI, EBX

//ESI := EDI + roldword(dword(ESI + (EBX xor (EDI or (not ECX))) + Data[14] + $ab9423a7), 15);
    mov EAX, ECX
    add ESI, $ab9423a7
    not EAX
    add ESI, [EDX + 4*14]
    or EAX, EDI
    xor EAX, EBX
    add ESI, EAX
    rol ESI, 15
    add ESI, EDI

//ECX := ESI + roldword(dword(ECX + (EDI xor (ESI or (not EBX))) + Data[5] + $fc93a039), 21);
    mov EAX, EBX
    add ECX, $fc93a039
    not EAX
    add ECX, [EDX + 4*5]
    or EAX, ESI
    xor EAX, EDI
    add ECX, EAX
    rol ECX, 21
    add ECX, ESI

//EBX := ECX + roldword(dword(EBX + (ESI xor (ECX or (not EDI))) + Data[12] + $655b59c3),  6);
    mov EAX, EDI
    add EBX, $655b59c3
    not EAX
    add EBX, [EDX + 4*12]
    or EAX, ECX
    xor EAX, ESI
    add EBX, EAX
    rol EBX,  6
    add EBX, ECX

//EDI := EBX + roldword(dword(EDI + (ECX xor (EBX or (not ESI))) + Data[3] + $8f0ccc92), 10);
    mov EAX, ESI
    add EDI, $8f0ccc92
    not EAX
    add EDI, [EDX + 4*3]
    or EAX, EBX
    xor EAX, ECX
    add EDI, EAX
    rol EDI, 10
    add EDI, EBX

//ESI := EDI + roldword(dword(ESI + (EBX xor (EDI or (not ECX))) + Data[10] + $ffeff47d), 15);
    mov EAX, ECX
    add ESI, $ffeff47d
    not EAX
    add ESI, [EDX + 4*10]
    or EAX, EDI
    xor EAX, EBX
    add ESI, EAX
    rol ESI, 15
    add ESI, EDI

//ECX := ESI + roldword(dword(ECX + (EDI xor (ESI or (not EBX))) + Data[1] + $85845dd1), 21);
    mov EAX, EBX
    add ECX, $85845dd1
    not EAX
    add ECX, [EDX + 4*1]
    or EAX, ESI
    xor EAX, EDI
    add ECX, EAX
    rol ECX, 21
    add ECX, ESI

//EBX := ECX + roldword(dword(EBX + (ESI xor (ECX or (not EDI))) + Data[8] + $6fa87e4f),  6);
    mov EAX, EDI
    add EBX, $6fa87e4f
    not EAX
    add EBX, [EDX + 4*8]
    or EAX, ECX
    xor EAX, ESI
    add EBX, EAX
    rol EBX,  6
    add EBX, ECX

//EDI := EBX + roldword(dword(EDI + (ECX xor (EBX or (not ESI))) + Data[15] + $fe2ce6e0), 10);
    mov EAX, ESI
    add EDI, $fe2ce6e0
    not EAX
    add EDI, [EDX + 4*15]
    or EAX, EBX
    xor EAX, ECX
    add EDI, EAX
    rol EDI, 10
    add EDI, EBX

//ESI := EDI + roldword(dword(ESI + (EBX xor (EDI or (not ECX))) + Data[6] + $a3014314), 15);
    mov EAX, ECX
    add ESI, $a3014314
    not EAX
    add ESI, [EDX + 4*6]
    or EAX, EDI
    xor EAX, EBX
    add ESI, EAX
    rol ESI, 15
    add ESI, EDI

//ECX := ESI + roldword(dword(ECX + (EDI xor (ESI or (not EBX))) + Data[13] + $4e0811a1), 21);
    mov EAX, EBX
    add ECX, $4e0811a1
    not EAX
    add ECX, [EDX + 4*13]
    or EAX, ESI
    xor EAX, EDI
    add ECX, EAX
    rol ECX, 21
    add ECX, ESI

//EBX := ECX + roldword(dword(EBX + (ESI xor (ECX or (not EDI))) + Data[4] + $f7537e82),  6);
    mov EAX, EDI
    add EBX, $f7537e82
    not EAX
    add EBX, [EDX + 4*4]
    or EAX, ECX
    xor EAX, ESI
    add EBX, EAX
    rol EBX,  6
    add EBX, ECX

//EDI := EBX + roldword(dword(EDI + (ECX xor (EBX or (not ESI))) + Data[11] + $bd3af235), 10);
    mov EAX, ESI
    add EDI, $bd3af235
    not EAX
    add EDI, [EDX + 4*11]
    or EAX, EBX
    xor EAX, ECX
    add EDI, EAX
    rol EDI, 10
    add EDI, EBX

//ESI := EDI + roldword(dword(ESI + (EBX xor (EDI or (not ECX))) + Data[2] + $2ad7d2bb), 15);
    mov EAX, ECX
    add ESI, $2ad7d2bb
    not EAX
    add ESI, [EDX + 4*2]
    or EAX, EDI
    xor EAX, EBX
    add ESI, EAX
    rol ESI, 15
    add ESI, EDI

//ECX := ESI + roldword(dword(ECX + (EDI xor (ESI or (not EBX))) + Data[9] + $eb86d391), 21);
    mov EAX, EBX
    add ECX, $eb86d391
    not EAX
    add ECX, [EDX + 4*9]
    or EAX, ESI
    xor EAX, EDI
    add ECX, EAX
    rol ECX, 21
    add ECX, ESI

    pop EAX // EAX = Context
    add TMDContext.State[EAX + 4*0], EBX //  Context.State[0 .. 3] += A, B, C, D
    add TMDContext.State[EAX + 4*1], ECX
    add TMDContext.State[EAX + 4*2], ESI
    add TMDContext.State[EAX + 4*3], EDI

//Inc(Context.Length,64);
    add	dword ptr TMDContext.Length[EAX],64
    adc	dword ptr TMDContext.Length[EAX + 4],0

    pop EDI
    pop ESI
    pop EBX
end;
