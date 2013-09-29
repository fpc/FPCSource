{ %NORUN }

program tw22225;

{$mode objfpc}
type
  TT = class
  public 
    type 
      TErr = class 
      end;
  end;
begin
  try
  except on TT.TErr do;
  end;
end.
