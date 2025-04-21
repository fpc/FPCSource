{$push}
{$asmmode intel}

{$define DO_DISPATCH}

{$if defined( CPUX86_HAS_SSE4_2)}
{ $if defined}
{$endif}

procedure {$ifdef DO_DISPATCH}SHA1Transform_Sha1Asm{$else}SHA1Transform{$endif}
      (var Ctx: TSHA1Context; const Data: Pointer); assembler;
{$if defined(x86_64) and not defined(windows)}nostackframe;{$endif}
{$ifndef CPU64}
const MASK : packed record a, b : qword; end = (a:$08090a0b0c0d0e0f;b:$0001020304050607);
var
  ABCD_SAVE,E0_SAVE : packed record a, b : qword; end;
{$endif}
{$if defined(x86_64) and defined(windows)}
var sav6,sav7,sav8,sav9 : record a,b:qword end; {abi for win64 requires to save and restore xmm6-xmm15}
{$endif}
{   Registers used  }
{  E0        = xmm0 }
{  E1        = xmm1 }
{  ABCD      = xmm2 }
{  SHUF_MASK = xmm3 }
{  MSG0      = xmm4 }
{  MSG1      = xmm5 }
{  MSG2      = xmm6 }
{  MSG3      = xmm7 }
{  ABCD_SAVE = xmm8  (only on x86_64) }
{  E0_SAVE   = xmm9  (only on x86_64) }
asm
{$if defined(x86_64) and defined(windows)}
   movdqu sav6,xmm6
   movdqu sav7,xmm7
   movdqu sav8,xmm8
   movdqu sav9,xmm9
{$endif}

    //* Load initial values */
    pxor   xmm0,xmm0   {important, we later write register bits[127..96]  }
    movdqu xmm2,[Ctx]
    pshufd xmm2,xmm2,$1b
    pinsrd xmm0,dword ptr [Ctx+16],$03      { instruction set SSE4.2 }
    {$ifdef CPU64}
    mov rax,$0001020304050607;
    pinsrq xmm3,rax,$01
    mov rax,$08090a0b0c0d0e0f;
    pinsrq xmm3,rax,$00
    {$else}
    movdqu xmm3,[mask]
    {$endif}

    //while (length >= 64) do
    //begin

        //* Save current state  */
        {$ifdef CPU64}
        movdqu xmm8,xmm2         {ABCD_SAVE = ABCD;}
        movdqu xmm9,xmm0         {E0_SAVE = E0;}
        {$else}
        movdqu [ABCD_SAVE],xmm2  {ABCD_SAVE = ABCD;}
        movdqu [E0_SAVE],xmm0    {E0_SAVE = E0;}
        {$endif}

        //* Rounds 0-3 */
        movdqu xmm4,oword ptr[Data] {MSG0 = [Data+00]}
        pshufb xmm4,xmm3      {MSG0 = shuffle(MSG0, MASK);}
        paddd  xmm0,xmm4      {E0 = add (E0, MSG0);}
        movdqa xmm1,xmm2      {E1 = ABCD;}
        sha1rnds4 xmm2,xmm0,0 {ABCD = sha1rnds4(ABCD, E0, 0);}

        //* Rounds 4-7 */
        movdqu xmm5,oword ptr[Data+16] {MSG1 = [Data+16]}
        pshufb xmm5,xmm3      {MSG1 = shuffle(MSG1, MASK);}
        sha1nexte xmm1,xmm5   {E1 = sha1nexte(E1, MSG1);}
        movdqa xmm0,xmm2      {E0 = ABCD;}
        sha1rnds4 xmm2,xmm1,0 {ABCD = sha1rnds4(ABCD, E1, 0);}
        sha1msg1 xmm4,xmm5    {MSG0 = sha1msg1(MSG0, MSG1);}

        //* Rounds 8-11 */
        movdqu xmm6,oword ptr[Data+32] {MSG2 = [Data+32]}
        pshufb xmm6,xmm3      {MSG2 = shuffle(MSG2, MASK);}
        sha1nexte xmm0,xmm6   {E0 = sha1nexte(E0, MSG2);}
        movdqu xmm1,xmm2      {E1 = ABCD;}
        sha1rnds4 xmm2,xmm0,0 {ABCD = sha1rnds4(ABCD, E0, 0);}
        sha1msg1 xmm5,xmm6    {MSG1 = sha1msg1(MSG1, MSG2);}
        pxor xmm4,xmm6        {MSG0 = xor(MSG0, MSG2);}

        //* Rounds 12-15 */
        movdqu xmm7,oword ptr[Data+48] {MSG3 = [Data+48]}
        pshufb xmm7,xmm3      {MSG3 = shuffle(MSG3, MASK);}
        sha1nexte xmm1,xmm7   {E1 = sha1nexte(E1, MSG3);}
        movdqu xmm0,xmm2      {E0 = ABCD;}
        sha1msg2 xmm4,xmm7    {MSG0 = sha1msg2(MSG0, MSG3);}
        sha1rnds4 xmm2,xmm1,0 {ABCD = sha1rnds4(ABCD, E1, 0);}
        sha1msg1 xmm6,xmm7    {MSG2 = sha1msg1(MSG2, MSG3);}
        pxor xmm5,xmm7        {MSG1 = xor(MSG1, MSG3);}

        //* Rounds 16-19 */
        sha1nexte xmm0,xmm4   {E0 = sha1nexte(E0, MSG0);}
        movdqu xmm1,xmm2      {E1 = ABCD;}
        sha1msg2 xmm5,xmm4    {MSG1 = sha1msg2(MSG1, MSG0);}
        sha1rnds4 xmm2,xmm0,0 {ABCD = sha1rnds4(ABCD, E0, 0);}
        sha1msg1 xmm7,xmm4    {MSG3 = sha1msg1(MSG3, MSG0);}
        pxor xmm6,xmm4        {MSG2 = xor(MSG2, MSG0);}

        //* Rounds 20-23 */
        sha1nexte xmm1,xmm5   {E1 = sha1nexte(E1, MSG1);}
        movdqu xmm0,xmm2      {E0 = ABCD;}
        sha1msg2 xmm6,xmm5    {MSG2 = sha1msg2(MSG2, MSG1);}
        sha1rnds4 xmm2,xmm1,1 {ABCD = sha1rnds4(ABCD, E1, 1);}
        sha1msg1 xmm4,xmm5    {MSG0 = sha1msg1(MSG0, MSG1);}
        pxor xmm7,xmm5        {MSG3 = xor(MSG3, MSG1);}

        //* Rounds 24-27 */
        sha1nexte xmm0,xmm6   {E0 = sha1nexte(E0, MSG2);}
        movdqu xmm1,xmm2      {E1 = ABCD;}
        sha1msg2 xmm7,xmm6    {MSG3 = sha1msg2(MSG3, MSG2);}
        sha1rnds4 xmm2,xmm0,1 {ABCD = sha1rnds4(ABCD, E0, 1);}
        sha1msg1 xmm5,xmm6    {MSG1 = sha1msg1(MSG1, MSG2);}
        pxor xmm4,xmm6        {MSG0 = xor(MSG0, MSG2);}

        //* Rounds 28-31 */
        sha1nexte xmm1,xmm7   {E1 = sha1nexte(E1, MSG3);}
        movdqu xmm0,xmm2      {E0 = ABCD;}
        sha1msg2 xmm4,xmm7    {MSG0 = sha1msg2(MSG0, MSG3);}
        sha1rnds4 xmm2,xmm1,1 {ABCD = sha1rnds4(ABCD, E1, 1);}
        sha1msg1 xmm6,xmm7    {MSG2 = sha1msg1(MSG2, MSG3);}
        pxor xmm5,xmm7        {MSG1 = xor(MSG1, MSG3);}

        //* Rounds 32-35 */
        sha1nexte xmm0,xmm4   {E0 = sha1nexte(E0, MSG0);}
        movdqu xmm1,xmm2      {E1 = ABCD;}
        sha1msg2 xmm5,xmm4    {MSG1 = sha1msg2(MSG1, MSG0);}
        sha1rnds4 xmm2,xmm0,1 {ABCD = sha1rnds4(ABCD, E0, 1);}
        sha1msg1 xmm7,xmm4    {MSG3 = sha1msg1(MSG3, MSG0);}
        pxor xmm6,xmm4        {MSG2 = xor(MSG2, MSG0);}

        //* Rounds 36-39 */
        sha1nexte xmm1,xmm5   {E1 = sha1nexte(E1, MSG1);}
        movdqu xmm0,xmm2      {E0 = ABCD;}
        sha1msg2 xmm6,xmm5    {MSG2 = sha1msg2(MSG2, MSG1);}
        sha1rnds4 xmm2,xmm1,1 {ABCD = sha1rnds4(ABCD, E1, 1);}
        sha1msg1 xmm4,xmm5    {MSG0 = sha1msg1(MSG0, MSG1);}
        pxor xmm7,xmm5        {MSG3 = xor(MSG3, MSG1);}

        //* Rounds 40-43 */
        sha1nexte xmm0,xmm6   {E0 = sha1nexte(E0, MSG2);}
        movdqu xmm1,xmm2      {E1 = ABCD;}
        sha1msg2 xmm7,xmm6    {MSG3 = sha1msg2(MSG3, MSG2);}
        sha1rnds4 xmm2,xmm0,2 {ABCD = sha1rnds4(ABCD, E0, 2);}
        sha1msg1 xmm5,xmm6    {MSG1 = sha1msg1(MSG1, MSG2);}
        pxor xmm4,xmm6        {MSG0 = xor(MSG0, MSG2);}

        //* Rounds 44-47 */
        sha1nexte xmm1,xmm7   {E1 = sha1nexte(E1, MSG3);}
        movdqu xmm0,xmm2      {E0 = ABCD;}
        sha1msg2 xmm4,xmm7    {MSG0 = sha1msg2(MSG0, MSG3);}
        sha1rnds4 xmm2,xmm1,2 {ABCD = sha1rnds4(ABCD, E1, 2);}
        sha1msg1 xmm6,xmm7    {MSG2 = sha1msg1(MSG2, MSG3);}
        pxor xmm5,xmm7        {MSG1 = xor(MSG1, MSG3);}

        //* Rounds 48-51 */
        sha1nexte xmm0,xmm4   {E0 = sha1nexte(E0, MSG0);}
        movdqu xmm1,xmm2      {E1 = ABCD;}
        sha1msg2 xmm5,xmm4    {MSG1 = sha1msg2(MSG1, MSG0);}
        sha1rnds4 xmm2,xmm0,2 {ABCD = sha1rnds4(ABCD, E0, 2);}
        sha1msg1 xmm7,xmm4    {MSG3 = sha1msg1(MSG3, MSG0);}
        pxor xmm6,xmm4        {MSG2 = xor(MSG2, MSG0);}

        //* Rounds 52-55 */
        sha1nexte xmm1,xmm5   {E1 = sha1nexte(E1, MSG1);}
        movdqu xmm0,xmm2      {E0 = ABCD;}
        sha1msg2 xmm6,xmm5    {MSG2 = sha1msg2(MSG2, MSG1);}
        sha1rnds4 xmm2,xmm1,2 {ABCD = sha1rnds4(ABCD, E1, 2);}
        sha1msg1 xmm4,xmm5    {MSG0 = sha1msg1(MSG0, MSG1);}
        pxor xmm7,xmm5        {MSG3 = xor(MSG3, MSG1);}

        //* Rounds 56-59 */
        sha1nexte xmm0,xmm6   {E0 = sha1nexte(E0, MSG2);}
        movdqu xmm1,xmm2      {E1 = ABCD;}
        sha1msg2 xmm7,xmm6    {MSG3 = sha1msg2(MSG3, MSG2);}
        sha1rnds4 xmm2,xmm0,2 {ABCD = sha1rnds4(ABCD, E0, 2);}
        sha1msg1 xmm5,xmm6    {MSG1 = sha1msg1(MSG1, MSG2);}
        pxor xmm4,xmm6        {MSG0 = xor(MSG0, MSG2);}

        //* Rounds 60-63 */
        sha1nexte xmm1,xmm7   {E1 = sha1nexte(E1, MSG3);}
        movdqu xmm0,xmm2      {E0 = ABCD;}
        sha1msg2 xmm4,xmm7    {MSG0 = sha1msg2(MSG0, MSG3);}
        sha1rnds4 xmm2,xmm1,3 {ABCD = sha1rnds4(ABCD, E1, 3);}
        sha1msg1 xmm6,xmm7    {MSG2 = sha1msg1(MSG2, MSG3);}
        pxor xmm5,xmm7        {MSG1 = xor(MSG1, MSG3);}

        //* Rounds 64-67 */
        sha1nexte xmm0,xmm4   {E0 = sha1nexte(E0, MSG0);}
        movdqu xmm1,xmm2      {E1 = ABCD;}
        sha1msg2 xmm5,xmm4    {MSG1 = sha1msg2(MSG1, MSG0);}
        sha1rnds4 xmm2,xmm0,3 {ABCD = sha1rnds4(ABCD, E0, 3);}
        sha1msg1 xmm7,xmm4    {MSG3 = sha1msg1(MSG3, MSG0);}
        pxor xmm6,xmm4        {MSG2 = xor(MSG2, MSG0);}

        //* Rounds 68-71 */
        sha1nexte xmm1,xmm5   {E1 = sha1nexte(E1, MSG1);}
        movdqu xmm0,xmm2      {E0 = ABCD;}
        sha1msg2 xmm6,xmm5    {MSG2 = sha1msg2(MSG2, MSG1);}
        sha1rnds4 xmm2,xmm1,3 {ABCD = sha1rnds4(ABCD, E1, 3);}
        pxor xmm7,xmm5        {MSG3 = xor(MSG3, MSG1);}

        //* Rounds 72-75 */
        sha1nexte xmm0,xmm6   {E0 = sha1nexte(E0, MSG2);}
        movdqu xmm1,xmm2      {E1 = ABCD;}
        sha1msg2 xmm7,xmm6    {MSG3 = sha1msg2(MSG3, MSG2);}
        sha1rnds4 xmm2,xmm0,3 {ABCD = sha1rnds4(ABCD, E0, 3);}

        //* Rounds 76-79 */
        sha1nexte xmm1,xmm7   {E1 = sha1nexte(E1, MSG3);}
        movdqu xmm0,xmm2      {E0 = ABCD;}
        sha1rnds4 xmm2,xmm1,3 {ABCD = sha1rnds4(ABCD, E1, 3);}

        //* Combine state */
        {$ifdef CPU64}
        sha1nexte xmm0,xmm9
        paddd xmm2,xmm8
        {$else}
        sha1nexte xmm0,[E0_SAVE]
        //paddd xmm2,[ABCD_SAVE]
        movdqu xmm4,[ABCD_SAVE]
        paddd xmm2,xmm4
        {$endif}

        //data += 64;
        //length -= 64;
    //end;

    //* Save state */
    pshufd xmm2,xmm2,$1b  {ABCD = shuffle(ABCD, 0x1B);}
    pextrd dword ptr [Ctx+16],xmm0,$03 {Ctx.State[4] = E0 }
    movdqu [Ctx],xmm2     {Ctx.State[0..3] = ABCD;}
    {$ifdef CPU64}
    ADD   QWORD PTR [ctx.Length], 64 { ctx.Length += 64 }
    {$else}
    ADD   DWORD PTR [ctx.Length  ], 64 { ctx.Length += 64 }
    ADC   DWORD PTR [ctx.Length+4],  0
    {$endif}
{$if defined(x86_64) and defined(windows)}
   movdqu xmm6,sav6
   movdqu xmm7,sav7
   movdqu xmm8,sav8
   movdqu xmm9,sav9
{$endif}
end;

{$pop}


{$ifdef DO_DISPATCH}
procedure SHA1Transform_Dispatch(var Ctx: TSHA1Context; const Data: Pointer); forward;

var
  SHA1Transform_Impl: procedure (var Ctx: TSHA1Context; const Data: Pointer) = @SHA1Transform_Dispatch;

procedure SHA1Transform_Dispatch(var Ctx: TSHA1Context; const Data: Pointer);
begin
  if SSE42Support and SHASupport then
    SHA1Transform_Impl:=@SHA1Transform_Sha1Asm
  else
    SHA1Transform_Impl:=@SHA1Transform_Plain;
  SHA1Transform_Impl(Ctx,Data);
end;

procedure SHA1Transform(var Ctx: TSHA1Context; const Data: Pointer);
begin
  SHA1Transform_Impl(Ctx,Data);
end;
{$endif DO_DISPATCH}
