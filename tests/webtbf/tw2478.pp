{ %fail }

{ Source provided for Free Pascal Bug Report 2478 }
{ Submitted by "Louis Jean-Richard" on  2003-04-30 }
{ e-mail: Ljean_richard@compuserve.com }
UNIT tw2478;
{$HINTS OFF}
INTERFACE
TYPE
        aByteArray = ARRAY[1 .. 100] OF byte;
        aCardinalArray = ARRAY[4 .. 100] OF byte;
        aType1 = ARRAY[12 .. 100] OF byte;
TYPE
        anObject =
                OBJECT
                        PUBLIC
                                PROCEDURE Method( CONST aba: aByteArray );
                                PROCEDURE Method( CONST clt: aType1 );
                        PRIVATE
                                PROCEDURE Method( CONST aba: aByteArray; CONST clt: aType1 );
                                PROCEDURE Method( CONST aca: aCardinalArray; CONST clt: aType1 );
                                PROCEDURE Method( cp, cc: cardinal; CONST clt: aType1 );
                END
                ;
{       *************************************************************** }
IMPLEMENTATION
PROCEDURE anObject.Method( CONST aba: aByteArray );
BEGIN END;
PROCEDURE anObject.Method( CONST aba: aByteArray; CONST clt: aType1 );
BEGIN END;
PROCEDURE anObject.Method( CONST aca: aCardinalArray; CONST clt: aType1 );
BEGIN END;
PROCEDURE anObject.Method( CONST clt: aType1 );
BEGIN END;
INITIALIZATION
FINALIZATION
END
.
