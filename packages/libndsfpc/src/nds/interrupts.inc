{$ifdef NDS_INTERFACE}
type
  IRQ_MASK = cuint;
const
  IRQ_VBLANK			    : IRQ_MASK =	(1 shl 0);		(* vertical blank interrupt mask *)
  IRQ_HBLANK			    : IRQ_MASK =	(1 shl 1);		(* horizontal blank interrupt mask *)
  IRQ_VCOUNT			    : IRQ_MASK =	(1 shl 2);		(* vcount match interrupt mask *)
  IRQ_TIMER0			    : IRQ_MASK =	(1 shl 3);		(* timer 0 interrupt mask *)
  IRQ_TIMER1			    : IRQ_MASK =	(1 shl 4);		(* timer 1 interrupt mask *)
  IRQ_TIMER2			    : IRQ_MASK =	(1 shl 5);		(* timer 2 interrupt mask *)
  IRQ_TIMER3			    : IRQ_MASK =	(1 shl 6);		(* timer 3 interrupt mask *)
  IRQ_NETWORK			    : IRQ_MASK =	(1 shl 7);		(* serial interrupt mask *)
  IRQ_DMA0			      : IRQ_MASK =	(1 shl 8);		(* DMA 0 interrupt mask *)
  IRQ_DMA1			      : IRQ_MASK =	(1 shl 9);		(* DMA 1 interrupt mask *)
  IRQ_DMA2			      : IRQ_MASK =	(1 shl 10);	  (* DMA 2 interrupt mask *)
  IRQ_DMA3			      : IRQ_MASK =	(1 shl 11);	  (* DMA 3 interrupt mask *)
  IRQ_KEYS            : IRQ_MASK =	(1 shl 12);	  (* Keypad interrupt mask *)
  IRQ_CART			      : IRQ_MASK =	(1 shl 13); 	(* GBA cartridge interrupt mask *)
  IRQ_IPC_SYNC		    : IRQ_MASK =	(1 shl 16);	  (* IPC sync interrupt mask *)
  IRQ_FIFO_EMPTY		  : IRQ_MASK =	(1 shl 17);	  (* Send FIFO empty interrupt mask *)
  IRQ_FIFO_NOT_EMPTY	: IRQ_MASK =	(1 shl 18);	  (* Receive FIFO empty interrupt mask *)
  IRQ_CARD			      : IRQ_MASK =	(1 shl 19);	  (* interrupt mask DS Card Slot *)
  IRQ_CARD_LINE		    : IRQ_MASK =	(1 shl 20);	  (* interrupt mask *)
  IRQ_GEOMETRY_FIFO   : IRQ_MASK =	(1 shl 21);	  (* geometry FIFO interrupt mask *)
  IRQ_LID				      : IRQ_MASK =	(1 shl 22);	  (* interrupt mask hinge *)
  IRQ_SPI				      : IRQ_MASK =	(1 shl 23);	  (* SPI interrupt mask *)
  IRQ_WIFI			      : IRQ_MASK =	(1 shl 24);	  (* WIFI interrupt mask (ARM7)*)
  IRQ_ALL				      : IRQ_MASK =	(not 0);      //$FFFFFF ?


type
  IRQ_MASKSAUX = cuint;
const
  IRQ_I2C   : IRQ_MASKSAUX = (1 shl 6); (* Power Button interrupt mask (DSi ARM7) *)
 	IRQ_SDMMC : IRQ_MASKSAUX = (1 shl 8); (* Sdmmc interrupt mask (DSi ARM7) *)

function IRQ_TIMER(n: cint): cint; inline;

const
  MAX_INTERRUPTS  = 25;



const
  REG_IE	 : pcuint32 = pointer($04000210);
  REG_AUXIE: pcuint32 = pointer($04000218);
  REG_IF	 : pcuint32 = pointer($04000214);
  REG_AUXIF: pcuint32 = pointer($0400021C);
  REG_IME	 : pcuint32 = pointer($04000208);

type
  IME_VALUE = integer;
const
  IME_DISABLE : IME_VALUE = 0;  (* Disable all interrupts. *)
  IME_ENABLE  : IME_VALUE = 1;  (* Enable all interrupts not masked out in REG_IE *)


type
  TVoidFn = procedure of object;
  
var
//  __irq_vector: array [0..0] of VoidFunctionPointer; cvar; external;
//  __irq_vector: array [0..0] of Pointer; cvar; external;
//  __irq_flags: array [0..0] of cuint32; cvar; external;
//  __irq_flags: pcuint32; cvar; external;
//  __irq_vector: ^VoidFunctionPointer; cvar; external;

///  __irq_vector: Pointer; cvar; external;
///  __irq_flags: pcuint32; cvar; external;
  __irq_vector: Pointer; external name '__irq_vector';
  __irq_flags: pcuint32; external name '__irq_flags';
  __irq_flagsaux: pcuint32; external name '__irq_flagsaux';


{$define INTR_WAIT_FLAGS := __irq_flags}
{$define INTR_WAIT_FLAGSAUX := __irq_flagsaux}
{$define IRQ_HANDLER := __irq_vector}

type
  IntTable = record
    handler: IntFn; 
    mask: cuint32;
  end;

procedure irqInit(); cdecl; external;
procedure irqSet(irq: cuint32; handler: TVoidFn); cdecl; external;
procedure irqSet(irq: cuint32; handler: pointer); cdecl; external;
procedure irqSetAUX(irq: cuint32; handler: TVoidFn); cdecl; external;
procedure irqSetAUX(irq: cuint32; handler: pointer); cdecl; external;
procedure irqClear(irq: cuint32); cdecl; external;
procedure irqClearAUX(irq: cuint32); cdecl; external;
procedure irqInitHandler(handler: TVoidFn); cdecl; external;
procedure irqInitHandler(handler: pointer); cdecl; external;
procedure irqEnable(irq: cuint32); cdecl; external;
procedure irqEnableAUX(irq: cuint32); cdecl; external;
procedure irqDisable(irq: cuint32); cdecl; external;
procedure irqDisableAUX(irq: cuint32); cdecl; external;

procedure swiIntrWait(waitForSet: cuint32; flags: cuint32); cdecl; external;
procedure swiWaitForVBlank(); cdecl; external;

function setPowerButtonCB(CB: TVoidFn): TVoidFn; cdecl; external;

function enterCriticalSection(): cint; inline;
procedure leaveCriticalSection(oldIME: cint); inline;
{$endif NDS_INTERFACE}

{$ifdef NDS_IMPLEMENTATION}

function IRQ_TIMER(n: cint): cint; inline;
begin
  result := (1 shl (n + 3));
end;

function enterCriticalSection(): cint; inline;
var
  oldIME: cint;
begin
  oldIME := REG_IME^;
  REG_IME^ := 0;
  result := oldIME;
end;

procedure leaveCriticalSection(oldIME: cint); inline;
begin
  REG_IME^ := oldIME;
end;

{$endif NDS_IMPLEMENTATION}

