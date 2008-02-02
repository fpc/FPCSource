(*
  $Id: interrupts.inc 25 2007-12-10 21:06:46Z p4p3r0 $
  ------------------------------------------------------------------------------
  	Copyright (C) 2005
  		Jason Rogers (dovoto)
  		Dave Murphy (WinterMute)
  	
  	This software is provided 'as-is', without any express or implied
  	warranty.  In no event will the authors be held liable for any
  	damages arising from the use of this software.
  	
  	Permission is granted to anyone to use this software for any
  	purpose, including commercial applications, and to alter it and
  	redistribute it freely, subject to the following restrictions:
  	
  	1.	The origin of this software must not be misrepresented; you
  		must not claim that you wrote the original software. If you use
  		this software in a product, an acknowledgment in the product
  		documentation would be appreciated but is not required.
  	
  	2.	Altered source versions must be plainly marked as such, and
  		must not be misrepresented as being the original software.
  	
  	3.	This notice may not be removed or altered from any source
  		distribution.
  ------------------------------------------------------------------------------
    
    
  Conversion by Legolas (http://itaprogaming.free.fr) for freepascal compiler
  (http://www.freepascal.org)
   
  Copyright (C) 2006  Francesco Lombardi
  Check http://sourceforge.net/projects/libndsfpc for updates
   
  ------------------------------------------------------------------------------

  $Log$

*)


{$ifdef NDS_INTERFACE}
type
  IRQ_MASKS = cuint;
  IRQ_MASK = IRQ_MASKS;
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
  IRQ_CARD			      : IRQ_MASK =	(1 shl 19);	  (* interrupt mask *)
  IRQ_CARD_LINE		    : IRQ_MASK =	(1 shl 20);	  (* interrupt mask *)
  IRQ_GEOMETRY_FIFO   : IRQ_MASK =	(1 shl 21);	  (* geometry FIFO interrupt mask *)
  IRQ_LID				      : IRQ_MASK =	(1 shl 22);	  (* interrupt mask *)
  IRQ_SPI				      : IRQ_MASK =	(1 shl 23);	  (* SPI interrupt mask *)
  IRQ_WIFI			      : IRQ_MASK =	(1 shl 24);	  (* WIFI interrupt mask (ARM7)*)
  IRQ_ALL				      : IRQ_MASK =	 $FFFFFF {not 0};

const
  MAX_INTERRUPTS  = 25;



const
  REG_IE	: pcuint32 = pointer($04000210);
  REG_IF	: pcuint32 = pointer($04000214);
  REG_IME	: pcuint16 = pointer($04000208);

type
  IME_VALUE = integer;
const
  IME_DISABLE : IME_VALUE = 0;  (* Disable all interrupts. *)
  IME_ENABLE  : IME_VALUE = 1;  (* Enable all interrupts not masked out in REG_IE *)


var
//  __irq_vector: array [0..0] of VoidFunctionPointer; cvar; external;
//  __irq_vector: array [0..0] of Pointer; cvar; external;
//  __irq_flags: array [0..0] of cuint32; cvar; external;
//  __irq_flags: pcuint32; cvar; external;
//  __irq_vector: ^VoidFunctionPointer; cvar; external;
  __irq_vector: Pointer; cvar; external;
  __irq_flags: pcuint32; cvar; external;



{$define VBLANK_INTR_WAIT_FLAGS := __irq_flags}
{$define IRQ_HANDLER := __irq_vector}

type
  IntTable = record
    handler: IntFn; 
    mask: cuint32;
  end;

procedure irqInit(); cdecl; external;
//procedure irqSet(irq: IRQ_MASK; handler: VoidFunctionPointer); cdecl; external;
procedure irqSet(irq: IRQ_MASK; handler: pointer); cdecl; external;
procedure irqClear(irq: IRQ_MASK); cdecl; external;
//procedure irqInitHandler(handler: VoidFunctionPointer); cdecl; external;
procedure irqInitHandler(handler: pointer); cdecl; external;
procedure irqEnable(irq: cuint32); cdecl; external;
procedure irqDisable(irq: cuint32); cdecl; external;
{$endif NDS_INTERFACE}
