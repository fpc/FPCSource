{ Old file: tbs0140.pp }
{ Shows that interdependent units still are not OK.     OK 0.99.6 (PFV) }

unit tb0120;

{
 The first compilation runs fine.
 A second compilation (i.e; .ppu files exist already) crashes the compiler !!
}

interface

type
 TObject = object
  constructor Init(aPar:byte);
 end;

implementation

uses ub0120;

constructor TObject.Init(aPar:byte);
 begin
  if aPar=0 then Message(Self);
 end;

end.
