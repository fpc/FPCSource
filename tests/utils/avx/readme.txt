create testfiles in shell (linux):

for i in `ls /tmp/avx/*.pp`; do /home/torsten/fpc/avx/ppcx64 -Fu/home/torsten/fpc/avx/rtl/units/x86_64-linux/ "$i"; done;


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


