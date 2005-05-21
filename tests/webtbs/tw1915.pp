PROGRAM SetIncl;
{
    This program demonstrates a set inclusion test bug.
    (After two days passed to track down a very perverse program error....)
    (By Louis Jean-Ruichard)
}
TYPE
    eAttr   = ( e0, e1, e2, e3, e4, e5, e6, e7 );
    entityP = ^entity;
    entity  =
            RECORD
                attr    : SET OF eAttr;
            END;
VAR
    ep  : entityP;
    e   : entity;
BEGIN
    e.attr:=[e2,e4,e7,e1,e0];
    WITH e DO
            IF ([e1,e0] <= attr)
            THEN Writeln('A1: [e1,e0] is included in attr')
    ;
    New(ep);
    ep^.attr:=[e2,e4,e7,e1,e0];
    WITH ep^ DO
            IF ([e1,e0] <= attr)
            THEN Writeln('A2: [e1,e0] is included in attr')
            ELSE
             begin
              Writeln('A2 statement incorrectly executed');
              Halt(1);
             end;
    ;
END.
