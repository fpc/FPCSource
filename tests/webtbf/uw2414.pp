{ Source provided for Free Pascal Bug Report 2414 }
{ Submitted by "Louis Jean-Richard" on  2003-03-10 }
{ e-mail: Ljean_richard@compuserve.com }
UNIT uw2414;
INTERFACE
TYPE
        anObj   =
                OBJECT
                        PROCEDURE A( CONST s : string );
                        k       : cardinal;
                        PRIVATE
                        PROCEDURE A( CONST s : string; n : word );
                END
                ;
IMPLEMENTATION
PROCEDURE anObj.A( CONST s : string; n : word );
BEGIN
        WriteLn(' ':n, s)
END
;
PROCEDURE anObj.A( CONST s : string );
BEGIN
        A(s, k)
END
;
END
.
