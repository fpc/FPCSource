program tw20996;

{$mode delphi}

uses
  uw20996;

type
  TRec = class
  type
    TInt = Integer;
    TNested = record
    const
      C = False;
    end;
  const
    C = True;
  end;

begin
  {$IF uw20996.V <> 123}
  halt(1);
  {$IFEND}
  {$IF NOT TRec.C}
  halt(2);
  {$IFEND}
  {$IF TRec.TNested.C}
  halt(3);
  {$IFEND}
  {$IF HIGH(TRec.TInt) <> High(Integer)}
  halt(4);
  {$IFEND}
end.
