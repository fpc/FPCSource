
{$IFDEF OGC_INTERFACE}
const
  IM_NONE = ( $ffffffff );
  IRQ_MEM0 = 0;
  IRQ_MEM1 = 1;
  IRQ_MEM2 = 2;
  IRQ_MEM3 = 3;
  IRQ_MEMADDRESS = 4;
  IRQ_DSP_AI = 5;
  IRQ_DSP_ARAM = 6;
  IRQ_DSP_DSP = 7;
  IRQ_AI = 8;
  IRQ_EXI0_EXI = 9;
  IRQ_EXI0_TC = 10;
  IRQ_EXI0_EXT = 11;
  IRQ_EXI1_EXI = 12;
  IRQ_EXI1_TC = 13;
  IRQ_EXI1_EXT = 14;
  IRQ_EXI2_EXI = 15;
  IRQ_EXI2_TC = 16;
  IRQ_PI_CP = 17;
  IRQ_PI_PETOKEN = 18;
  IRQ_PI_PEFINISH = 19;
  IRQ_PI_SI = 20;
  IRQ_PI_DI = 21;
  IRQ_PI_RSW = 22;
  IRQ_PI_ERROR = 23;
  IRQ_PI_VI = 24;
  IRQ_PI_DEBUG = 25;
  IRQ_PI_HSP = 26;

{$ifdef HW_RVL}
const
  IRQ_PI_ACR = 27;
{$endif HW_RVL}
const
  IRQ_MAX = 32;

function IRQMASK(irq: cint): cint; inline;

{$define IM_MEM0 := IRQMASK(IRQ_MEM0)}
{$define IM_MEM1 := IRQMASK(IRQ_MEM1)}
{$define IM_MEM2 := IRQMASK(IRQ_MEM2)}
{$define IM_MEM3 := IRQMASK(IRQ_MEM3)}
{$define IM_MEMADDRESS := IRQMASK(IRQ_MEMADDRESS)}
{$define IM_MEM := (IM_MEM0 or IM_MEM1 or IM_MEM2 or IM_MEM3 or IM_MEMADDRESS)}

{$define IM_DSP_AI := IRQMASK(IRQ_DSP_AI)}
{$define IM_DSP_ARAM := IRQMASK(IRQ_DSP_ARAM)}
{$define IM_DSP_DSP := IRQMASK(IRQ_DSP_DSP)}
{$define IM_DSP := (IM_DSP_AI or IM_DSP_ARAM or IM_DSP_DSP)}

{$define IM_AI := IRQMASK(IRQ_AI)}

{$define IM_EXI0_EXI := IRQMASK(IRQ_EXI0_EXI)}
{$define IM_EXI0_TC := IRQMASK(IRQ_EXI0_TC)}
{$define IM_EXI0_EXT := IRQMASK(IRQ_EXI0_EXT)}
{$define IM_EXI0 := (IM_EXI0_EXI or IM_EXI0_TC or IM_EXI0_EXT)}

{$define IM_EXI1_EXI := IRQMASK(IRQ_EXI1_EXI)}
{$define IM_EXI1_TC := IRQMASK(IRQ_EXI1_TC)}
{$define IM_EXI1_EXT := IRQMASK(IRQ_EXI1_EXT)}
{$define IM_EXI1 := (IM_EXI1_EXI or IM_EXI1_TC or IM_EXI1_EXT)}

{$define IM_EXI2_EXI := IRQMASK(IRQ_EXI2_EXI)}
{$define IM_EXI2_TC := IRQMASK(IRQ_EXI2_TC)}
{$define IM_EXI2 := (IM_EXI2_EXI or IM_EXI2_TC)}
{$define IM_EXI := (IM_EXI0 or IM_EXI1 or IM_EXI2)}

{$define IM_PI_CP := IRQMASK(IRQ_PI_CP)}
{$define IM_PI_PETOKEN := IRQMASK(IRQ_PI_PETOKEN)}
{$define IM_PI_PEFINISH := IRQMASK(IRQ_PI_PEFINISH)}
{$define IM_PI_SI := IRQMASK(IRQ_PI_SI)}
{$define IM_PI_DI := IRQMASK(IRQ_PI_DI)}
{$define IM_PI_RSW := IRQMASK(IRQ_PI_RSW)}
{$define IM_PI_ERROR := IRQMASK(IRQ_PI_ERROR)}
{$define IM_PI_VI := IRQMASK(IRQ_PI_VI)}
{$define IM_PI_DEBUG := IRQMASK(IRQ_PI_DEBUG)}
{$define IM_PI_HSP := IRQMASK(IRQ_PI_HSP)}
{$ifdef HW_DOL}
  {$define IM_PI := (IM_PI_CP or IM_PI_PETOKEN or IM_PI_PEFINISH or IM_PI_SI or IM_PI_DI or IM_PI_RSW or IM_PI_ERROR or IM_PI_VI or IM_PI_DEBUG or IM_PI_HSP)}
{$else}
{$ifdef HW_RVL}
  {$define IM_PI_ACR := IRQMASK(IRQ_PI_ACR)}
  {$define IM_PI := (IM_PI_CP or IM_PI_PETOKEN or IM_PI_PEFINISH or IM_PI_SI or IM_PI_DI or IM_PI_RSW or IM_PI_ERROR or IM_PI_VI or IM_PI_DEBUG or IM_PI_HSP or IM_PI_ACR)}
{$else}  
  // And default code here  
{$endif}  
{$endif} 


type
  raw_irq_handler_t = procedure(irq: cuint32; ctx: pointer); cdecl;

function IRQ_Request(nIrq: cuint32; pHndl: raw_irq_handler_t; pCtx: pointer)
  : raw_irq_handler_t; cdecl; external;

function IRQ_Free(nIrq: cuint32): raw_irq_handler_t; cdecl; external;

function IRQ_GetHandler(nIrq: cuint32): raw_irq_handler_t; cdecl; external;

function IRQ_Disable: cuint32; cdecl; external;

procedure IRQ_Restore(level: cuint32); cdecl; external;


procedure __MaskIrq(nMask: cuint32); cdecl; external;

procedure __UnmaskIrq(nMask: cuint32); cdecl; external;
{$ENDIF OGC_INTERFACE}





{$IFDEF OGC_IMPLEMENTATION}
function IRQMASK(irq: cint): cint; inline;
begin
  IRQMASK := ($80000000 shr irq);
end;
{$ENDIF OGC_IMPLEMENTATION}

