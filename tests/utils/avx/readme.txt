create testfiles in shell (linux):

for i in `ls /tmp/avx/*.pp`; do /home/torsten/fpc/avx/ppcx64 -Fu/home/torsten/fpc/avx/rtl/units/x86_64-linux/ "$i"; done;

On windows, complete testing would look like:

i386:
mkdir tmp
fpc avxtestgenerator
fpc avxtestfilecmp
avxtestgenerator -ffpc -otmp
cd tmp
for %%p in (*.pp) do call fpc %%p -v0
cd ..
avxtestgenerator -fnasm -otmp
cd tmp
for %%a in (*.asm) do nasm -fwin32 %%a
cd ..
avxtestfilecmp -mtmp\*.obj -dtmp -eexe -s

x86_64:
fpc avxtestgenerator
fpc avxtestfilecmp
avxtestgenerator -px8664 -ffpc -otmp
cd tmp
for %%p in (*.pp) do call fpc -Px86_64 %%p -v0
cd ..
avxtestgenerator -px8664 -fnasm -otmp
cd tmp
for %%a in (*.asm) do nasm -fwin64 %%a
cd ..
avxtestfilecmp -mtmp\*.obj -dtmp -eexe -s

****************************************************************************************************
Note:
While avxtestgenerator supports FASM output, testing with FASM does not work because
FASM assembles slightly different from FPC, e.g. it swaps base and index register
in references if this is benefical (e.g. [rbp+rax] => [rax+rbp] saves one byte).
****************************************************************************************************

compare binary-files:

any instructions can have a different binary-streams

e.g.
     VMOVAPD XMM0, XMM1:

     possible binary-codes:
     VMOVAPD xmm1, xmm2/m128     [VEX.128.66.0F.28 /r]
     VMOVAPD xmm2/m128, xmm1     [VEX.128.66.0F.29 /r]


     VMOVSD XMM1, XMM2, XMM3

     possible binary-codes:
     VMOVSD xmm1, xmm2, xmm3     [VEX.NDS.LIG.F2.0F.WIG.10 /r] operand encoding: RVM
     VMOVSD xmm1, xmm2, xmm3     [VEX.NDS.LIG.F2.0F.WIG.11 /r] operand encoding: MVR

currently (AVX I):

VMOVAPD
VMOVAPS
VMOVDQA
VMOVDQU
VMOVQ
VMOVSD
VMOVSS
VMOVUPD
VMOVUPS


