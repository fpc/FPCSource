create testfiles in shell (linux):

for i in `ls /tmp/avx/*.pp`; do /home/torsten/fpc/avx/ppcx64 -Fu/home/torsten/fpc/avx/rtl/units/x86_64-linux/ "$i"; done;

***********************************************************************************************************
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
avxtestgenerator -px8664 -fnasm -otmp
cd tmp
for %%p in (*.pp) do call fpc -Px86_64 %%p -v0
for %a in (*.asm) do nasm -fwin64 %a
cd ..
avxtestfilecmp -mtmp\*.obj -dtmp -eexe -s

***********************************************************************************************************
Linux x86-64:

mkdir tmp
fpc avxtestgenerator
fpc avxtestfilecmp

./avxtestgenerator -px8664 -ffpc -otmp
# AVX-512: ./avxtestgenerator -px8664 -ffpc -otmp -z

./avxtestgenerator -px8664 -fnasm -otmp
# AVX-512: ./avxtestgenerator -px8664 -fnasm -otmp -z

cd tmp

# use GNU Parallel [1]
# if not available:
# echo *.pp | xargs -n 1 fpc -Px86_64 -v0i
find . -name '*.pp' | parallel fpc -Px86_64 -v0i

# use GNU Parallel [1]
# if not available:
# echo *.asm | xargs -n 1 nasm -fwin64
find . -name '*.asm' | parallel nasm -fwin64

cd ..
./avxtestfilecmp -mtmp/*.o -dtmp -s

***********************************************************************************************************
x86_64 testing by using self testing fpc executables: 

avxtestgenerator -px8664 -ffpcinc -otmp
avxtestgenerator -px8664 -fnasm -otmp
cd tmp
for %a in (*.asm) do nasm -fbin %a -o %~na.bin
for %b in (*.bin) do bin2obj -x -c %~nb %b -o%~nb.inc
for %p in (*.pp) do call fpc -Px86_64 %p -v0i
for %e in (*.exe) do call %e

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


[1] O. Tange (2011): GNU Parallel - The Command-Line Power Tool, ;login: The USENIX Magazine, February 2011:42-47.
