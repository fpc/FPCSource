PROGRAM def2def;
{ converts the rpoc section of a def file
  from function prototye format to function varaible format
  if not already in right form }

VAR
  Fin, Fout  : Text;
  I, J, K, L : Word;
  S, T, U, V : String;
  Mode       : Word;

PROCEDURE UpString(VAR S:String);
VAR
  I : Word;
BEGIN
  FOR I := 1 TO Length(S) DO
    S[I] := UpCase(S[I]);
END;

BEGIN
  IF ParamCount<>2 THEN
  BEGIN
    WriteLn('Error: Invalid parameter count');
    WriteLn('Usage:');
    WriteLn('  def2def [source] [destination]');
    Halt(1);
  END;

  IF ParamStr(1)=ParamStr(2) THEN
  BEGIN
    WriteLn('Error: files must be different');
    WriteLn('Usage:');
    WriteLn('  def2def [source] [destination]');
    Halt(1);
  END;

  Assign(Fin,ParamStr(1));
  Reset(Fin);

  Assign(Fout,ParamStr(2));
  ReWrite(Fout);

  WriteLn('Converting...');
  Mode := 0;
  WHILE Not(EOF(Fin)) DO
  BEGIN
    ReadLn(Fin,S);
    IF Length(S)>0 THEN
      IF S[1]='%' THEN
      BEGIN
        IF S='%END'            THEN Mode:=0 ELSE
        IF S='%COPY_INTERFACE' THEN Mode:=1 ELSE
        IF S='%PROCS'          THEN Mode:=2 ELSE
           (* Unknown *)            Mode:=0;
      END ELSE
      BEGIN
        CASE Mode OF
          0 : { nothing };
          1 : { nothing };
          2 : BEGIN
                T := S;
                WHILE Pos(' ',T)=1 DO
                  Delete(T,1,1);
                I := Pos(' ',T);
                U := Copy(T,1,I-1);
                V := U;
                UpString(U);
                IF (U='PROCEDURE') OR (U='FUNCTION') THEN
                BEGIN
                  { this line needs swapping }
                  Delete(T,1,I);
                  WHILE Pos(' ',T)=1 DO
                    Delete(T,1,1);
                  I := Pos('(',T);
                  J := Pos(' ',T);
                  K := Pos(':',T);
                  L := Pos(';',T);
                  IF L>0 THEN
                  BEGIN
                    IF (I>0) AND (I<L) THEN L := I;
                    IF (J>0) AND (J<L) THEN L := J;
                    IF (K>0) AND (K<L) THEN L := K;
                    Insert(': '+V,T,L);
                    S := T;
                  END;
                END;
              END;
          ELSE { nothing };
        END;
      END;
    WriteLn(Fout,S);
  END;

  Close(Fin);
  Close(Fout);

  WriteLn('Done.');
END.
