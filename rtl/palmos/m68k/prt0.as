This file isn't ready yet, we use the C startup code in crt0.o instead.

c:\FPC\FIXES\RTL\PALMOS\CRT0.O(.text+0x46):crt0.c: undefined reference to `_GccR
elocateData'
c:\FPC\FIXES\RTL\PALMOS\CRT0.O(.text+0x46):crt0.c: relocation truncated to fit:
DISP16 _GccRelocateData
c:\FPC\FIXES\RTL\PALMOS\CRT0.O(.text+0x50):crt0.c: undefined reference to `__do_
bhook'
c:\FPC\FIXES\RTL\PALMOS\CRT0.O(.text+0x50):crt0.c: relocation truncated to fit:
DISP16 __do_bhook
c:\FPC\FIXES\RTL\PALMOS\CRT0.O(.text+0x5a):crt0.c: undefined reference to `__do_
ctors'
c:\FPC\FIXES\RTL\PALMOS\CRT0.O(.text+0x5a):crt0.c: relocation truncated to fit:
DISP16 __do_ctors
c:\FPC\FIXES\RTL\PALMOS\CRT0.O(.text+0x70):crt0.c: undefined reference to `__do_
dtors'
c:\FPC\FIXES\RTL\PALMOS\CRT0.O(.text+0x70):crt0.c: relocation truncated to fit:
DISP16 __do_dtors
c:\FPC\FIXES\RTL\PALMOS\CRT0.O(.text+0x7a):crt0.c: undefined reference to `__do_
ehook'
c:\FPC\FIXES\RTL\PALMOS\CRT0.O(.text+0x7a):crt0.c: relocation truncated to fit:
DISP16 __do_ehook
TEST.O(.text+0x6):test.pas: undefined reference to `FPC_INITIALIZEUNITS'
TEST.O(.text+0x10):test.pas: undefined reference to `FPC_DO_EXIT'

Disassembly of section .text:

00000000 <start>:
   0:	4e56 fff4      	linkw %fp,#-12
   4:	48e7 1f00      	moveml %d3-%d7,%sp@-
   8:	486e fffc      	pea %fp@(-4)
   c:	486e fff8      	pea %fp@(-8)
  10:	486e fff4      	pea %fp@(-12)
  14:	4e4f           	trap #15
  16:	a08f           	0120217
  18:	4fef 000c      	lea %sp@(12),%sp
  1c:	4a40           	tstw %d0
  1e:	670e           	beqs 2e <start+0x2e>
  20:	1f3c 0003      	moveb #3,%sp@-
  24:	4e4f           	trap #15
  26:	a234           	0121064
  28:	70ff           	moveq #-1,%d0
  2a:	6000 0062      	braw 8e <start+0x8e>
  2e:	206e fff4      	moveal %fp@(-12),%a0
  32:	3c10           	movew %a0@,%d6
  34:	2a28 0002      	movel %a0@(2),%d5
  38:	3828 0006      	movew %a0@(6),%d4
  3c:	3604           	movew %d4,%d3
  3e:	0243 0004      	andiw #4,%d3
  42:	6704           	beqs 48 <start+0x48>
  44:	6100 ffba      	bsrw 0 <start>
  48:	3f04           	movew %d4,%sp@-
  4a:	2f05           	movel %d5,%sp@-
  4c:	3f06           	movew %d6,%sp@-
  4e:	6100 ffb0      	bsrw 0 <start>
  52:	508f           	addql #8,%sp
  54:	4a43           	tstw %d3
  56:	6704           	beqs 5c <start+0x5c>
  58:	6100 ffa6      	bsrw 0 <start>
  5c:	3f04           	movew %d4,%sp@-
  5e:	2f05           	movel %d5,%sp@-
  60:	3f06           	movew %d6,%sp@-
  62:	6100 ff9c      	bsrw 0 <start>
  66:	2e00           	movel %d0,%d7
  68:	508f           	addql #8,%sp
  6a:	4a43           	tstw %d3
  6c:	6704           	beqs 72 <start+0x72>
  6e:	6100 ff90      	bsrw 0 <start>
  72:	3f04           	movew %d4,%sp@-
  74:	2f05           	movel %d5,%sp@-
  76:	3f06           	movew %d6,%sp@-
  78:	6100 ff86      	bsrw 0 <start>
  7c:	2f2e fffc      	movel %fp@(-4),%sp@-
  80:	2f2e fff8      	movel %fp@(-8),%sp@-
  84:	2f2e fff4      	movel %fp@(-12),%sp@-
  88:	4e4f           	trap #15
  8a:	a090           	0120220
  8c:	2007           	movel %d7,%d0
  8e:	4cee 00f8 ffe0 	moveml %fp@(-32),%d3-%d7
  94:	4e5e           	unlk %fp
  96:	4e75           	rts
