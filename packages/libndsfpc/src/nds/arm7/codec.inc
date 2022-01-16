{$ifdef NDS_INTERFACE}
{$ifndef ARM7}
  {$error DSi TSC is only available on the ARM7}
{$endif ARM7}

function cdcIsAvailable(): boolean; inline;

type
  cdcBanks = integer;
const
  CDC_CONTROL: cdcBanks = $00; // Chip control
  CDC_SOUND: cdcBanks = $01; // ADC/DAC control
  CDC_TOUCHCNT: cdcBanks = $03; // TSC control
  CDC_TOUCHDATA: cdcBanks = $FC; // TSC data buffer


// Direct register functions
function cdcReadReg(bank, reg: cuint8): cuint8; cdecl; external;
procedure cdcReadRegArray(bank, reg: cuint8; data: pointer; size: cuint8); cdecl; external;
procedure cdcWriteReg(bank, reg, value: cuint8); cdecl; external;
procedure cdcWriteRegMask(bank, reg, mask, value: cuint8); cdecl; external;
procedure cdcWriteRegArray(bank, reg: cuint8; data: pointer; size: cuint8); cdecl; external;

// Touchscreen functions
procedure cdcTouchInit(); cdecl; external;
function cdcTouchPenDown(): cbool; cdecl; external;
function cdcTouchRead(pos: PtouchPosition): cbool; cdecl; external;

{$endif NDS_INTERFACE}

{$ifdef NDS_IMPLEMENTATION}

function cdcIsAvailable(): boolean; inline; 
begin
	cdcIsAvailable := isDSiMode() and ((__DSiHeader^.appflags and $01) <> 0);
end;

{$endif NDS_IMPLEMENTATION}