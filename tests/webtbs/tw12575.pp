{$mode objfpc}
PROGRAM Test;
USES SysUtils;

VAR
  t : Text;

BEGIN
  Assign(t, 'blah.txt');
  TRY
    Close(t);
  EXCEPT
    ON e: EInOutError DO
      if (e.ErrorCode <> 103) then
        halt(1);
  END;
END.
