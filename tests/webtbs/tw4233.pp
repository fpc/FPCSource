{ Source provided for Free Pascal Bug Report 4233 }
{ Submitted by "christian franz" on  2005-07-28 }
{ e-mail: cfranz@access.ch }

{$mode macpas}

type
  Tslave = object;

  TMaster = object
     mySlave : Tslave;
     next : TMaster;
  end;

  TSlave = object
     myMaster : TMaster;
     next : TSlave;
  end;

begin
end.
