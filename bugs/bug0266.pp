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
		   