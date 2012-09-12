{ %norun }

program tw22860;

{$ifdef fpc}
{$mode delphi}
{$endif}

type
  TZoneID=(
    zNone=-1,
    zSystem=zNone, // LC system parameters
    zOven=0, // GC or LC column temperature
    zInj=$10000,
    zInjA=zInj,
    zInjB=zInj + 1,
    zInjC=zInj + 2,
    zInjD=zInj + 3,
    zDet=$20000, // GC and LC detectors
    zDetA=zDet,
    zDetB=zDet + 1,
    zDetC=zDet + 2,
    zDetD=zDet + 3,
    zAux=$30000,
    zAux1=zAux,
    zAux2=zAux + 1,
    zAux3=zAux + 2,
    //LC
    zPump=zInj,
    zPumpA=zInjA,
    zPumpB=zInjB,
    zPumpC=zInjC,
    zPumpD=zInjD,

    zEvents  = $00040000, //events manager zone
    zSampler = $00050000,  // autosampler
    zDevice  = $40000000,  // the device itself
    zBlock   = $20000000  // the block of device
  ); 
  
begin
end.

