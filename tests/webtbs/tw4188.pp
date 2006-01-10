{ Source provided for Free Pascal Bug Report 4188 }
{ Submitted by "guy simon" on  2005-07-14 }
{ e-mail: gsimon2@wanadoo.fr }
PROGRAM CODESTRING ;
VAR

 A, B, C : STRING;
 I, N : BYTE ;

BEGIN
 RANDSEED := 3455;
 A :='AZERTYUIOP0123456';
 N := LENGTH(A);
 WRITELN('SOURCE STRING : ',A);

{ NOW CODING A INTO B }
 B := '' ;
 FOR I := 1 TO N DO B:= B + CHR ( ORD(A[I]) XOR RANDOM(256) );
 WRITELN('CODED STRING : ',B);

{ NOW DECODING B INTO C}
 RANDSEED := 3455;
 C := '';
 FOR I :=1 TO N DO C:= C + CHR ( ORD(B[I]) XOR RANDOM(256) );
 WRITELN('DECODED STRING : ',C);

 if C<>A then
   halt(1);
END.
