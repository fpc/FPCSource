{ Source provided for Free Pascal Bug Report 2442 }
{ Submitted by "Louis Jean-Richard" on  2003-03-28 }
{ e-mail: Ljean_richard@compuserve.com }
PROGRAM Procall;
TYPE
        anObject        =
                OBJECT
                        PROCEDURE A( w : word );
                        PROCEDURE A( c : cardinal );
                        n       : byte;
                END
                ;
PROCEDURE anObject.A( w : word );

        PROCEDURE B;
        BEGIN
                WriteLn('B called (word)')
        END
        ;
BEGIN
        n:=w DIV 2;
        B
END
;
PROCEDURE anObject.A( c : cardinal );

        PROCEDURE B;
        BEGIN
                WriteLn('B called (cardinal)');
        writeln('error!');
        halt(1);
        END
        ;
BEGIN
        n:=c DIV 4;
        B
END
;
VAR
        x       : anObject;
        w       : word;
BEGIN
        w:=1;
        x.A(w)  { the wrong local procedure is called !!! }
END
.
