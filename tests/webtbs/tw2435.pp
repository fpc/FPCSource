{ %OPT=-Sew -vw }

{ Source provided for Free Pascal Bug Report 2435 }
{ Submitted by "Louis Jean-Richard" on  2003-03-24 }
{ e-mail: Ljean_richard@compuserve.com }
PROGRAM Warning;
TYPE
  anObject  =
    OBJECT
      n : word;
      PROCEDURE Init;
    END
    ;
PROCEDURE anObject.Init;
BEGIN
  n:=0
END
;
VAR
  r1, r2  :
    RECORD
      a, b : anObject
    END
    ;
BEGIN
  WITH r1 DO BEGIN
    a.Init; b.Init;
  END
  ;
  r2.a.Init; { well, it's just being initialised ! }
  r2.b.Init;
END
.
