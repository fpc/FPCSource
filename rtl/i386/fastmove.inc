{$ifndef FPC_SYSTEM_HAS_MOVE}
{$define FPC_SYSTEM_HAS_MOVE}

{ at least valgrind up to 3.3 has a bug which prevents the default code to
  work so we use a rather simple implementation here }
procedure Move_8OrMore_Valgrind; assembler; nostackframe;
{ eax = source, edx = dest, ecx = count (ecx >= 8).
  If FPC_PIC: ebx pushed. }
asm
{$ifndef FPC_PIC}
    push   %ebx
{$endif}
    sub    %edx, %eax
    jae    .LForward
    mov    %ecx, %ebx
    add    %eax, %ebx            { eax is negative => ecx+eax > 0 if regions overlap }
    jb     .LBack                { if no overlap, still do forward move }

.LForward:
{$ifdef FPC_ENABLED_CLD}
    cld
{$endif FPC_ENABLED_CLD}
    push   %esi
    push   %edi
    lea    (%eax,%edx), %esi
    mov    %edx, %edi
    rep movsb
    pop    %edi
    pop    %esi
    pop    %ebx
    ret

.LBack:
    add    %ecx, %edx
.LNextb:
    dec    %edx
    mov    (%eax,%edx), %bl
    mov    %bl, (%edx)
    dec    %ecx
    jnz    .LNextb
    pop    %ebx
end;

procedure Move_8OrMore_IA32; assembler; nostackframe;
{ eax = source, edx = dest, ecx = count (ecx >= 8).
  If FPC_PIC: ebx pushed. }
asm
    fildq  (%eax)                { First and last 8 bytes, used both in .L33OrMore and ladder ending (.L9to16). }
    fildq  -8(%eax,%ecx)
    cmp    $16, %ecx
    jle    .L9to16
    cmp    $32, %ecx
    jg     .L33OrMore
    fildq  8(%eax)
    fildq  -16(%eax,%ecx)
    fistpq -16(%edx,%ecx)
    fistpq 8(%edx)
.L9to16:
    fistpq -8(%edx,%ecx)         { 9–16 bytes }
    fistpq (%edx)
{$ifdef FPC_PIC}
    pop    %ebx
{$endif}
    ret

.Lcancel:
    fucompp                      { Pop two elements loaded at the beginning. }
{$ifdef FPC_PIC}
    pop    %ebx
{$endif}
    ret
    .byte  {$ifndef FPC_PIC}102,{$endif}102,102,102,102,102,102,102,102,144 { Turns .balign 16 before .Lloop16f into a no-op. }

.L33OrMore:
    sub    %edx, %eax            { eax = src - dest }
    jz     .Lcancel              { exit if src=dest }
{$ifndef FPC_PIC}
    push   %ebx
{$endif}
    jnb    .LForward             { src>dest => forward move }

    mov    %ecx, %ebx
    add    %eax, %ebx            { eax is negative => ecx+eax > 0 if regions overlap }
    jb     .Lback                { if no overlap, still do forward move }

.LForward:
    mov    %edx, %ebx            { remember original dest to write first 16 bytes }
    add    %edx, %ecx            { Move dest to the next 16-byte boundary. +16 if already aligned, as first 16 bytes will be writen separately anyway. }
    add    $8, %edx
    and    $-8, %edx
    sub    %edx, %ecx

    sub    $16, %ecx
    jbe    .LPost16f

    .balign 16                   { no-op }
.Lloop16f:
    fildq  (%eax,%edx)
    fistpq (%edx)
    fildq  8(%eax,%edx)
    fistpq 8(%edx)
    add    $16, %edx
    sub    $16, %ecx
    ja     .Lloop16f

.LPost16f:                       { +16 fixup not applied after 16× loop, ecx = remaining - 16 here. }
    cmp    $-8, %ecx
    jle    .LFirstAndLast8f
    fildq  (%eax,%edx)
    fistpq (%edx)
.LFirstAndLast8f:
    fistpq 8(%edx,%ecx)          { Write first and last 8 bytes after everything else. }
    fistpq (%ebx)                { Important for <8-byte step between src and dest. }
    pop    %ebx
    ret
    .byte  102,102,102,102,102,102,102,144 { Turns .balign 16 before .Lloop16b into a no-op. }

{ backwards move }
.Lback:
    lea    -1(%edx,%ecx), %ebx   { points to the end of dest; remember to write last 8 bytes }
    mov    %ebx, %ecx            { move dest to the previous 8-byte boundary... }
    and    $-8, %ecx
    sub    %edx, %ecx
    add    %ecx, %edx

    sub    $16, %ecx
    jbe    .LPost16b

    .balign 16                   { no-op }
.Lloop16b:
    sub    $16, %edx
    fildq  8(%eax,%edx)
    fistpq 8(%edx)
    fildq  (%eax,%edx)
    fistpq (%edx)
    sub    $16, %ecx
    ja     .Lloop16b

.LPost16b:
    cmp    $-8, %ecx
    jle    .LFirstAndLast8b
    fildq  -8(%eax,%edx)
    fistpq -8(%edx)
.LFirstAndLast8b:
    sub    %ecx, %edx
    fistpq -7(%ebx)
    fistpq -16(%edx)
    pop    %ebx
end;

procedure Move_8OrMore_MMX; assembler; nostackframe;
{ eax = source, edx = dest, ecx = count (ecx >= 8).
  If FPC_PIC: ebx pushed. }
asm
    cmp    $72, %ecx             { Size at which using MMX becomes worthwhile. }
    jl     Move_8OrMore_IA32
{$ifndef FPC_PIC}
    push   %ebx
{$endif}
    movq   (%eax), %mm4          { First and last 8 bytes. }
    movq   -8(%eax,%ecx), %mm5
    sub    %edx, %eax            { eax = src - dest }
    jz     .Lquit                { exit if src=dest }
    jnb    .LForward             { src>dest => forward move }

    mov    %ecx, %ebx
    add    %eax, %ebx            { eax is negative => ecx+eax > 0 if regions overlap }
    jb     .Lback                { if no overlap, still do forward move }

.LForward:
    mov    %edx, %ebx            { remember original dest to write first 16 bytes }
    add    %edx, %ecx            { Move dest to the next 16-byte boundary. +16 if already aligned, as first 16 bytes will be writen separately anyway. }
    add    $8, %edx
    and    $-8, %edx
    sub    %edx, %ecx

    sub    $16, %ecx
    jbe    .LPost16f

    .balign 16
.Lloop16f:
    movq   (%eax,%edx), %mm0
    movq   %mm0, (%edx)
    movq   8(%eax,%edx), %mm0
    movq   %mm0, 8(%edx)
    add    $16, %edx
    sub    $16, %ecx
    ja     .Lloop16f

.LPost16f:                       { +16 fixup not applied after 16× loop, ecx = remaining - 16 here. }
    cmp    $-8, %ecx
    jle    .LFirstAndLast8f
    movq   (%eax,%edx), %mm0
    movq   %mm0, (%edx)
.LFirstAndLast8f:
    movq   %mm5, 8(%edx,%ecx)    { Write first and last 8 bytes after everything else. }
    movq   %mm4, (%ebx)          { Important for <8-byte step between src and dest. }
.Lquit:
    emms
    pop    %ebx
    ret
    .byte  102,102,102,102,102,102,102,102,102,102,102,102,102,144 { Turns .balign 16 before .Lloop16b into a no-op. }

{ backwards move }
.Lback:
    lea    -1(%edx,%ecx), %ebx   { points to the end of dest; remember to write last 8 bytes }
    mov    %ebx, %ecx            { move dest to the previous 8-byte boundary... }
    and    $-8, %ecx
    sub    %edx, %ecx
    add    %ecx, %edx

    sub    $16, %ecx
    jbe    .LPost16b

    .balign 16                   { no-op }
.Lloop16b:
    sub    $16, %edx
    movq   8(%eax,%edx), %mm0
    movq   %mm0, 8(%edx)
    movq   (%eax,%edx), %mm0
    movq   %mm0, (%edx)
    sub    $16, %ecx
    ja     .Lloop16b

.LPost16b:
    cmp    $-8, %ecx
    jle    .LFirstAndLast8b
    movq   -8(%eax,%edx), %mm0
    movq   %mm0, -8(%edx)
.LFirstAndLast8b:
    sub    %ecx, %edx
    movq   %mm4, -16(%edx)
    movq   %mm5, -7(%ebx)
    emms
    pop    %ebx
end;

{$ifndef FASTMOVE_DISABLE_SSE}
procedure Move_8OrMore_SSE; assembler; nostackframe;
{ eax = source, edx = dest, ecx = count (ecx >= 8).
  If FPC_PIC: ebx pushed. }
const
  ErmsThreshold = 1536;
  NtThreshold = 256 * 1024; { this limit must be processor-specific (1/2 L2 cache size) }
  PrefetchDistance = 512;
asm
    cmp    $16, %ecx
    jle    .L9to16
    movups (%eax), %xmm4         { First and last 16 bytes, used both in .L33OrMore and 17–32 branch. }
    movups -16(%eax,%ecx), %xmm5
    cmp    $32, %ecx
    jg     .L33OrMore
    movups %xmm4, (%edx)         { 17–32 bytes }
    movups %xmm5, -16(%edx,%ecx)
{$ifdef FPC_PIC}
    pop    %ebx
{$endif}
    ret

.L9to16:
    movq   (%eax), %xmm0
    movq   -8(%eax,%ecx), %xmm1
    movq   %xmm0, (%edx)
    movq   %xmm1, -8(%edx,%ecx)
.Lquit:
{$ifdef FPC_PIC}
    pop    %ebx
{$endif}
    ret
    .byte  {$ifndef FPC_PIC}102,{$endif}102,102,102,102,102,102,102,102,102,102,102,102,102,144 { Turns .balign 16 before .Lloop32f into a no-op. }

.L33OrMore:
    sub    %edx, %eax            { eax = src - dest }
    jz     .Lquit                { exit if src=dest }
{$ifndef FPC_PIC}
    push   %ebx
{$endif}
    jnb    .LForward             { src>dest => forward move }

    mov    %ecx, %ebx
    add    %eax, %ebx            { eax is negative => ecx+eax > 0 if regions overlap }
    jb     .Lback                { if no overlap, still do forward move }

.LForward:
    mov    %edx, %ebx            { remember original dest to write first 16 bytes }
    add    %edx, %ecx            { Move dest to the next 16-byte boundary. +16 if already aligned, as first 16 bytes will be writen separately anyway. }
    add    $16, %edx
    and    $-16, %edx
    sub    %edx, %ecx

.LRestAfterNTf:
    sub    $32, %ecx             { During the N× loop, ecx is N bytes less than actually remained to allow sub N+jae .LLoop instead of sub N+cmp N+jae .LLoop. }
    jbe    .LPost32f
    cmp    $NtThreshold-32, %ecx
    jae    .Lntf                 { might jump back right away after more checks, but the branch is taken only on huge moves so it's better to take these checks out of here... }
.LNtIsNotBetter:
    cmp    $ErmsThreshold-32, %ecx { Even enhanced REP MOV does not seem to use NT so falls behind on huge moves. So prioritize NT. }
    jae    .LRepMovsF
.LRepMovsIsNotBetter:
    test   $15, %eax
    jz     .Lalignedloop32f

    .balign 16                   { no-op }
.Lloop32f:
    movups (%eax,%edx), %xmm0
    movaps %xmm0, (%edx)
    movups 16(%eax,%edx), %xmm0
    movaps %xmm0, 16(%edx)
    add    $32, %edx
    sub    $32, %ecx
    ja     .Lloop32f

.LPost32f:                       { +32 fixup not applied after 32× loop, ecx = remaining - 32 here. }
    cmp    $-16, %ecx
    jle    .LFirstAndLast16f
    movups (%eax,%edx), %xmm0
    movaps %xmm0, (%edx)
.LFirstAndLast16f:
    movups %xmm5, 16(%edx,%ecx)  { Write first and last 16 bytes after everything else. }
    movups %xmm4, (%ebx)         { Important for <16-byte step between src and dest. }
    pop    %ebx
    ret

    .balign 16
.Lalignedloop32f:                { Same as above starting from .Lloop32f but with MOVAPSes. }
    movaps (%eax,%edx), %xmm0
    movaps %xmm0, (%edx)
    movaps 16(%eax,%edx), %xmm0
    movaps %xmm0, 16(%edx)
    add    $32, %edx
    sub    $32, %ecx
    ja     .Lalignedloop32f

.LalignedPost32f:
    cmp    $-16, %ecx
    jle    .LalignedFirstAndLast16f
    movaps (%eax,%edx), %xmm0
    movaps %xmm0, (%edx)
.LalignedFirstAndLast16f:
    movups %xmm5, 16(%edx,%ecx)
    movups %xmm4, (%ebx)
    pop    %ebx
    ret

.LRepMovsF:
{$ifdef FPC_PIC}
    push   %ebx
    call   fpc_geteipasebx
    addl   $_GLOBAL_OFFSET_TABLE_, %ebx
    movl   fast_large_repmovstosb@GOT(%ebx), %ebx
    cmpb   $1, (%ebx)
    pop    %ebx
{$else FPC_PIC}
    cmpb   $1, fast_large_repmovstosb
{$endif FPC_PIC}
    jne    .LRepMovsIsNotBetter
    push   %esi
    push   %edi
    lea    (%eax,%edx), %esi
    mov    %edx, %edi
    add    $32, %ecx
    rep movsb
    movups %xmm4, (%ebx)         { last 16 aren't required }
    pop    %edi
    pop    %esi
    pop    %ebx
    ret

.Lntf:
    cmp    $NtThreshold, %eax    { Maybe change mind: don't bother bypassing cache if src and dest are close to each other }
    jb     .LNtIsNotBetter       { (this check is performed here to not stand in the way of smaller counts) }
    sub    $PrefetchDistance+32, %ecx { ecx = remaining - prefetch distance - bytes per loop (64), but 32 was subtracted already. }
    test   $15, %eax
    jz     .Lalignedntloop64f

    .balign 16
.Lntloop64f:
    prefetchnta 0+PrefetchDistance(%eax,%edx,1)
    movups (%eax,%edx,1), %xmm0
    movntps %xmm0, (%edx)
    movups 16(%eax,%edx,1), %xmm0
    movntps %xmm0, 16(%edx)
    movups 32(%eax,%edx,1), %xmm0
    movntps %xmm0, 32(%edx)
    movups 48(%eax,%edx,1), %xmm0
    movntps %xmm0, 48(%edx)
    add    $64, %edx
    sub    $64, %ecx
    jae    .Lntloop64f

    sfence
    add    $PrefetchDistance+64, %ecx
    jmp    .LRestAfterNTf        { go handle remaining bytes }

    .balign 16
.Lalignedntloop64f:              { Same as above starting from .Lntloop64f but with MOVAPSes. }
    prefetchnta 0+PrefetchDistance(%eax,%edx,1)
    movaps (%eax,%edx,1), %xmm0
    movntps %xmm0, (%edx)
    movaps 16(%eax,%edx,1), %xmm0
    movntps %xmm0, 16(%edx)
    movaps 32(%eax,%edx,1), %xmm0
    movntps %xmm0, 32(%edx)
    movaps 48(%eax,%edx,1), %xmm0
    movntps %xmm0, 48(%edx)
    add    $64, %edx
    sub    $64, %ecx
    jae    .Lalignedntloop64f

    sfence
    add    $PrefetchDistance+64, %ecx
    jmp    .LRestAfterNTf
    .byte  102,102,102,102,102,144 { Turns .balign 16 before .Lloop32b into a no-op. }

{ backwards move }
.Lback:
    lea    -1(%edx,%ecx), %ebx   { points to the end of dest; remember to write last 16 bytes }
    mov    %ebx, %ecx            { move dest to the previous 16-byte boundary... }
    and    $-16, %ecx
    sub    %edx, %ecx
    add    %ecx, %edx

.LRestAfterNTb:
    sub    $32, %ecx
    jbe    .LPost32b
    cmp    $NtThreshold-32, %ecx
    jae    .Lntb

    .balign 16                   { no-op }
.Lloop32b:
    sub    $32, %edx
    movups 16(%eax,%edx), %xmm0
    movaps %xmm0, 16(%edx)
    movups (%eax,%edx), %xmm0
    movaps %xmm0, (%edx)
    sub    $32, %ecx
    ja     .Lloop32b

.LPost32b:
    cmp    $-16, %ecx
    jle    .LFirstAndLast16b
    movups -16(%eax,%edx), %xmm0
    movaps %xmm0, -16(%edx)
.LFirstAndLast16b:
    sub    %ecx, %edx
    movups %xmm4, -32(%edx)
    movups %xmm5, -15(%ebx)
    pop    %ebx
    ret

.Lntb:
    cmp    $-NtThreshold, %eax
    jnb    .Lloop32b
    sub    $PrefetchDistance+32, %ecx

    .balign 16
.Lntloop64b:
    prefetchnta -PrefetchDistance(%eax,%edx,1)
    sub    $64, %edx
    movups 48(%eax,%edx,1), %xmm0
    movntps %xmm0, 48(%edx)
    movups 32(%eax,%edx,1), %xmm0
    movntps %xmm0, 32(%edx)
    movups 16(%eax,%edx,1), %xmm0
    movntps %xmm0, 16(%edx)
    movups (%eax,%edx,1), %xmm0
    movntps %xmm0, (%edx)
    sub    $64, %ecx
    jae    .Lntloop64b

    sfence
    add    $PrefetchDistance+64, %ecx
    jmp    .LRestAfterNTb
end;
{$endif ndef FASTMOVE_DISABLE_SSE}

procedure Move_8OrMore_Dispatch; forward;

var
  fastmoveproc : pointer = @Move_8OrMore_Dispatch;
{$ifndef FPC_HAS_INDIRECT_ENTRY_INFORMATION}
  valgrind_used : boolean;external name '__fpc_valgrind';
{$endif FPC_HAS_INDIRECT_ENTRY_INFORMATION}

function Move_8OrMore_HumanFriendlyDispatch: pointer;
begin
  { workaround valgrind bug }
{$ifdef FPC_HAS_INDIRECT_ENTRY_INFORMATION}
  if EntryInformation.valgrind_used then
{$else FPC_HAS_INDIRECT_ENTRY_INFORMATION}
  if valgrind_used then
{$endif FPC_HAS_INDIRECT_ENTRY_INFORMATION}
    result:=@Move_8OrMore_Valgrind
{$ifndef FASTMOVE_DISABLE_SSE}
  else if has_sse_support then
    result:=@Move_8OrMore_SSE
{$endif ndef FASTMOVE_DISABLE_SSE}
  else if has_mmx_support then
    result:=@Move_8OrMore_MMX
  else
    result:=@Move_8OrMore_IA32;
  if fpc_cpucodeinit_performed then
    fastmoveproc:=result;
end;

procedure Move_8OrMore_Dispatch; assembler; nostackframe;
{ eax = source, edx = dest, ecx = count (ecx >= 8).
  If FPC_PIC: ebx pushed. }
asm
{$ifndef FPC_PIC}
    push %ebx
{$endif}
    push %eax
    push %edx
    push %ecx
    call Move_8OrMore_HumanFriendlyDispatch
    mov  %eax, %ebx
    pop  %ecx
    pop  %edx
    pop  %eax
{$ifdef FPC_PIC}
    jmp  %ebx
{$else}
    call %ebx
    pop  %ebx
{$endif}
end;

procedure Move(const source;var dest;count:SizeInt); [public, alias: 'FPC_MOVE']; assembler; nostackframe;
asm
    cmp    $8, %ecx
    jle    .L8OrLess
{$ifdef FPC_PIC}
    push   %ebx
    call   fpc_geteipasebx
    addl   $_GLOBAL_OFFSET_TABLE_, %ebx
    movl   fastmoveproc@GOT(%ebx), %ebx
    jmp    (%ebx)
{$else}
    jmp    fastmoveproc
{$endif}

.L8OrLess:
    cmp    $3, %ecx
    jle    .L3OrLess
    push   %ebx
    mov    (%eax), %ebx
    mov    -4(%eax,%ecx), %eax
    mov    %ebx, (%edx)
    mov    %eax, -4(%edx,%ecx)
    pop    %ebx
    ret

.L3OrLess:
    cmp    $1, %ecx
    jl     .LZero
    push   %ebx
    movzbl (%eax), %ebx
    je     .LOne
    movzwl -2(%eax,%ecx), %eax
    mov    %ax, -2(%edx,%ecx)
.LOne:
    mov    %bl, (%edx)
    pop    %ebx
.LZero:
end;

{$endif  FPC_SYSTEM_HAS_MOVE}
