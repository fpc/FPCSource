{ Old file: tbs0268.pp }
{ crash with exceptions                                OK 0.99.13 (FK) }

PROGRAM Test2;

{$MODE DELPHI}

USES SysUtils;  // Dos for DosError because FindFirst is not a Function?

PROCEDURE DirList;
(* Show all Files, gives me "unhandled exception occurred at xxx, access
   violation" after inserting Try Except it worked but i got a "forever
   scrolling screen", then i inserted raise and got a correct "Exception
   in FindFirst" and "At end of ExceptionAddressStack"
   Next i inserted the ON E:EXCEPTION and ,E.Message an got 9999 *)
VAR SR : TSearchRec;
BEGIN
  TRY
    FindFirst ('*',faAnyFile,SR);  // why not a function ?
  EXCEPT
    ON E:EXCEPTION DO
      WriteLn ('Exception in FindFirst !-', E.Message);
  END;
  repeat
    Write (SR.Name,' ');
  until FindNext (SR)<>0;
  FindClose (SR);                  // and this is Delphi ?
END;

BEGIN
  WriteLn ('Hello, this is my first FPC-Program');
  DirList;
END.
