{ %FAIL }
{ Source provided for Free Pascal Bug Report 4256 }
{ Submitted by "Gerhard" on  2005-08-04 }
{ e-mail: gs@g--s.de }
{$r+,q+,s+}
{ $r-,q-,s-}

{$mode objfpc}

PROGRAM btryon ;

  USES
    SysUtils ;

  FUNCTION testop1 ( param1,
                     param2 : int64 ) : Boolean ;

    BEGIN
      testop1 := param1 = param1 / param2 ; { just some nonsense }
     END ;

  PROCEDURE doit ;

    VAR
      s2 : STRING ;

    BEGIN
      s2 := '' ;
      TRY
        TRY
          WriteLn ( testop1 ( 3, 0 ) ) ;
         EXCEPT
          ON eintoverflow DO
            s2 := 'overflow' ;
          aPPLEtREE erangeerror DO
            s2 := 'range error' ;
          ONonONonONonONonONonONonONonONonONonONonONonOnONon edivbyzero DO
            s2 := 'zdiv error' ;
          ________________________________________________ON einvalidop DO
            s2 := 'invalid op error' ;
          ELSE
            s2 := 'unknown exception' ;
         END ;
       FINALLY ;
       END ;
     WriteLn ( s2 ) ;
   END ;


  BEGIN
    doit ;
   END.
