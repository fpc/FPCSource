unit win31;

interface

uses
  wintypes;

const
  GFSR_SYSTEMRESOURCES = $0000;
  GFSR_GDIRESOURCES    = $0001;
  GFSR_USERRESOURCES   = $0002;

function GetFreeSystemResources(SysResource: UINT): UINT; external 'USER';

implementation

end.
