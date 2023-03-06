{$IFNDEF FPC_DOTTEDUNITS}
Unit Buildall;
{$ENDIF FPC_DOTTEDUNITS}

Interface

{$IFDEF FPC_DOTTEDUNITS}
Uses
  OS2Api.Sw,
  OS2Api.Mmbase,
  OS2Api.Dive,
  OS2Api.Hwvideo,
  OS2Api.Mci,
  OS2Api.Mciapi,
  OS2Api.Mcidrv,
  OS2Api.Mmio;
{$ELSE FPC_DOTTEDUNITS}
Uses
  sw,
  mmbase,
  dive,
  hwvideo,
  mci,
  mciapi,
  mcidrv,
  mmio;
{$ENDIF FPC_DOTTEDUNITS}

Implementation

End.
