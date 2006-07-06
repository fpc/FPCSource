{ Old file: tbs0266.pp }
{ linux crt write cuts 256 char                        OK 0.99.13 (PFV) }

{ %skiptarget=wince }

PROGRAM t10;

USES CRT;

VAR S: STRING;
    X: BYTE;


    BEGIN
       S := '';
          FOR X := 1 TO 253 DO S:=S+'-';
             S := S+'_!';
                WRITE(S);
                   WRITE('*',S);
                   END.

