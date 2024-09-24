//
// $PSLibId: Run-time Library Release 4.6$
//
{$MODE OBJFPC}
unit r3000;
interface
const

// Segment base addresses and sizes
	K0BASE = $80000000;
	K0SIZE = $20000000;
	K1BASE = $A0000000;
	K1SIZE = $20000000;
	K2BASE = $C0000000;
	K2SIZE = $20000000;

// Exception vectors
	UT_VEC = K0BASE;							// utlbmiss vector
	E_VEC  = K0BASE + $80;						// exception vector
	R_VEC  = K1BASE + $1fc00000;				// reset vector

// Address conversion macros
function K0_TO_K1(x: dword): dword;				// kseg0 to kseg1
function K1_TO_K0(x: dword): dword;				// kseg1 to kseg0
function K0_TO_PHYS(x: dword): dword;			// kseg0 to physical
function K1_TO_PHYS(x: dword): dword;			// kseg1 to physical
function PHYS_TO_K0(x: dword): dword;			// physical to kseg0
function PHYS_TO_K1(x: dword): dword;			// physical to kseg1

// Address predicates
function IS_KSEG0(x: dword): boolean;
function IS_KSEG1(x: dword): boolean;
// function IS_KSEG2(x: dword): boolean;
// function IS_KPTESEG(x: dword): boolean;
function IS_KUSEG(x: dword): boolean;


const
// Cache size constants
	MINCACHE =	4 * 1024;
	MAXCACHE =	64 * 1024;


// Status register
	SR_CUMASK =	$f0000000;					// coproc usable bits

	SR_CU3    =	$80000000;					// Coprocessor 3 usable
	SR_CU2    =	$40000000;					// Coprocessor 2 usable
	SR_CU1	  =	$20000000;					// Coprocessor 1 usable
	SR_CU0	  =	$10000000;					// Coprocessor 0 usable

	SR_BEV	  =	$00400000;					// use boot exception vectors

// Cache control bits
	SR_TS	  =	$00200000;					// TLB shutdown
	SR_PE	  =	$00100000;					// cache parity error
	SR_CM	  =	$00080000;					// cache miss
	SR_PZ	  =	$00040000;					// cache parity zero
	SR_SWC	  =	$00020000;					// swap cache
	SR_ISC	  =	$00010000;					// Isolate data cache

 	SR_MM_MODE = $00010000;					// lwl/swl/etc become scache/etc
 {
 define: 
 lcache		lwl
 scache		swl
 flush		lwr $0,
 inval		swr $0,
}


// Interrupt enable bits
// (NOTE: bits set to 1 enable the corresponding level interrupt)
	SR_IMASK	= $0000ff00;			// Interrupt mask
	SR_IMASK8	= $00000000;			// mask level 8
	SR_IMASK7	= $00008000;			// mask level 7
	SR_IMASK6	= $0000c000;			// mask level 6
	SR_IMASK5	= $0000e000;			// mask level 5
	SR_IMASK4	= $0000f000;			// mask level 4
	SR_IMASK3	= $0000f800;			// mask level 3
	SR_IMASK2	= $0000fc00;			// mask level 2
	SR_IMASK1	= $0000fe00;			// mask level 1
	SR_IMASK0	= $0000ff00;			// mask level 0

	SR_IBIT8	= $00008000;			// bit level 8
	SR_IBIT7	= $00004000;			// bit level 7
	SR_IBIT6	= $00002000;			// bit level 6
	SR_IBIT5	= $00001000;			// bit level 5
	SR_IBIT4	= $00000800;			// bit level 4
	SR_IBIT3	= $00000400;			// bit level 3
	SR_IBIT2	= $00000200;			// bit level 2
	SR_IBIT1	= $00000100;			// bit level 1

	SR_KUO		= $00000020;			// old kernel/user, 0 => k, 1 => u
	SR_IEO		= $00000010;			// old interrupt enable, 1 => enable
	SR_KUP		= $00000008;			// prev kernel/user, 0 => k, 1 => u
	SR_IEP		= $00000004;			// prev interrupt enable, 1 => enable
	SR_KUC		= $00000002;			// cur kernel/user, 0 => k, 1 => u
	SR_IEC		= $00000001;			// cur interrupt enable, 1 => enable

	SR_IMASKSHIFT	= 8;

	SR_FMT		= '\20\40BD\26TS\25PE\24CM\23PZ\22SwC\21IsC\20IM7\17IM6\16IM5\15IM4\14IM3\13IM2\12IM1\11IM0\6KUo\5IEo\4KUp\3IEp\2KUc\1IEc';

// Cause Register

	CAUSE_BD		= $80000000;		// Branch delay slot */
	CAUSE_CEMASK	= $30000000;		// coprocessor error */
	CAUSE_CESHIFT	= 28;

/// Interrupt pending bits
	CAUSE_IP8	= $00008000;			// External level 8 pending
	CAUSE_IP7	= $00004000;			// External level 7 pending
	CAUSE_IP6	= $00002000;			// External level 6 pending
	CAUSE_IP5	= $00001000;			// External level 5 pending
	CAUSE_IP4	= $00000800;			// External level 4 pending
	CAUSE_IP3	= $00000400;			// External level 3 pending
	CAUSE_SW2	= $00000200;			// Software level 2 pending
	CAUSE_SW1	= $00000100;			// Software level 1 pending

	CAUSE_IPMASK	= $0000FF00;		// Pending interrupt mask
	CAUSE_IPSHIFT	= 8;

	CAUSE_EXCMASK	= $0000003C;		// Cause code bits
	CAUSE_EXCSHIFT	= 2;

	CAUSE_FMT		= '\20\40BD\36CE1\35CE0\20IP8\17IP7\16IP6\15IP5\14IP4\13IP3\12SW2\11SW1\1INT';


// Cause register exception codes
function EXC_CODE(x: dword): dword;

const
// Hardware exception codes
	EXC_INT		= 0 shl 2;			// interrupt
	EXC_MOD		= 1 shl 2;			// TLB mod
	EXC_RMISS	= 2 shl 2;			// Read TLB Miss
	EXC_WMISS	= 3 shl 2;			// Write TLB Miss
	EXC_RADE	= 4 shl 2;			// Read Address Error
	EXC_WADE	= 5 shl 2;			// Write Address Error
	EXC_IBE		= 6 shl 2;			// Instruction Bus Error
	EXC_DBE		= 7 shl 2;			// Data Bus Error
	EXC_SYSCALL	= 8 shl 2;			// SYSCALL
	EXC_BREAK	= 9 shl 2;			// BREAKpoint
	EXC_II		= 10 shl 2;			// Illegal Instruction
	EXC_CPU		= 11 shl 2;			// CoProcessor Unusable
	EXC_OV		= 12 shl 2;			// OVerflow

// software exception codes
	SEXC_SEGV	= 16 shl 2;			// Software detected seg viol
	SEXC_RESCHED= 17 shl 2;			// resched request
	SEXC_PAGEIN	= 18 shl 2;			// page-in request
	SEXC_CPU	= 19 shl 2;			// coprocessor unusable


// Coprocessor 0 registers
{$DEFINE C0_INX	$0} 				// tlb index
{$DEFINE C0_RAND $1}				// tlb random
{$DEFINE C0_TLBLO $2}				// tlb entry low
{$DEFINE C0_CTXT $4}				// tlb context

{$DEFINE C0_PIDMASK	$6}				// Mips2

{$DEFINE C0_BADVADDR $8}			// bad virtual address

{$DEFINE C0_TLBHI $10}				// tlb entry hi
{$DEFINE C0_PID	$10}				// Mips2

{$DEFINE C0_SR $12}					// status register
{$DEFINE C0_CAUSE $13}				// exception cause
{$DEFINE C0_EPC $14}				// exception pc
{$DEFINE C0_PRID $15}				// revision identifier
{$DEFINE C0_ERREG $16}				// Mips2

// Coprocessor 0 operations
	C0_READI  	= $1;		// read ITLB entry addressed by C0_INDEX
	C0_WRITEI 	= $2;		// write ITLB entry addressed by C0_INDEX
	C0_WRITER 	= $6;		// write ITLB entry addressed by C0_RAND
	C0_PROBE  	= $8;		// probe for ITLB entry addressed by TLBHI
	C0_RFE	  	= $10;		// restore for exception

// Flags for the nofault handler. 0 means no fault is expected.
	NF_BADADDR	= 1;			// badaddr, wbadaddr
	NF_COPYIO	= 2;			// copyin, copyout
	NF_ADDUPC	= 3;			// addupc
	NF_FSUMEM	= 4;			// fubyte, subyte, fuword, suword
	NF_USERACC	= 5;			// useracc
	NF_SOFTFP	= 6;			// softfp
	NF_REVID	= 7;			// revision ids
	NF_NENTRIES	= 8;

// TLB size constants
 	TLBWIREDBASE    = 0;               					// WAG for now
 	NWIREDENTRIES   = 8;               					// WAG for now
 	TLBRANDOMBASE   = NWIREDENTRIES;
 	NTLBENTRIES     = 64;
 	NRANDOMENTRIES  = (NTLBENTRIES - NWIREDENTRIES);
 	              		

 	TLBRAND_RANDMASK   = $00003f00;
	TLBRAND_RANDSHIFT  = 8;


// Chip interrupt vector
	NC0VECS		 = 8;



// extern int (*c0vec_tbl[])();
var
	c0vec_tbl : array of function: longint; external;


const
 	BRK_KERNEL 		 = $f1;
 	EXCEPT_NORM      = 1;
 	EXCEPT_UTLB      = 2;
 	EXCEPT_BRKPT   	 = 3;
 	EXCEPT_DB    	 = 4;
 	EXCEPT_GDB    	 = 4;
	EXCEPT_INT    	 = 9;
	EXCEPT_ELSE    	 = $ff;



implementation

// Address conversion macros
function K0_TO_K1(x: dword): dword;	
begin
	result:= x or $A0000000;
end;

function K1_TO_K0(x: dword): dword;
begin
	result:= x and $9FFFFFFF;
end;

function K0_TO_PHYS(x: dword): dword;
begin
	result:= x and $1FFFFFFF;
end;

function K1_TO_PHYS(x: dword): dword;
begin
	result:= x and $1FFFFFFF;
end;

function PHYS_TO_K0(x: dword): dword;
begin
	result:= x or $80000000;
end;

function PHYS_TO_K1(x: dword): dword;
begin
	result:= x or $A0000000;
end;

// Address predicates
function IS_KSEG0(x: dword): boolean;
begin	
	result:= ((x >= K0BASE) and (x < K1BASE));
end;

function IS_KSEG1(x: dword): boolean;
begin
	result:= ((x >= K1BASE) and (x < K2BASE));
end;


{
function IS_KSEG2(x: dword): boolean;
begin
	result:= ((x >= K2BASE) and (x < KPTEBASE));
end;

function IS_KPTESEG(x: dword): boolean;
begin
	result:= (x >= KPTEBASE);
end;
}



function IS_KUSEG(x: dword): boolean;
begin
	result:= (x < K0BASE);
end;

function EXC_CODE(x: dword): dword;
begin
	result:= x shl 2;
end;

begin
end.