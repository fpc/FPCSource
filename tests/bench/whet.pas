program Whet;

{$IFDEF VirtualPascal}
{$AlignCode+,AlignData+,AlignRec+,Asm-,B-,Cdecl-,D-,Delphi-,Frame+,G4+,I-}
{$Optimise+,OrgName-,P-,Q-,R-,SmartLink+,Speed+,T-,V-,W-,X+,Z-,ZD-}
uses
  Dos, Os2Def, Os2Base;
{$ENDIF}

{$IFDEF Speed}
{$B-,D-,I-,L-,O-,Q-,R-,S-,V-,Z-}
uses
  Dos, BseDos;
{$ENDIF}

{$IFDEF Speed_Pascal_20}
{$B-,D-,I-,L-,O-,Q-,R-,S-,V-,Z-}
uses
  Dos,BseDos,OS2Def;
{$ENDIF}

{$IFDEF VER70}
{$A+,B-,D-,E-,F-,G+,I-,L-,N+,O-,P-,Q-,R-,S-,T-,V-,X-,Y-}
{$M 16384,0,655360}
uses
  OpTimer, Dos;
{$ENDIF}

{$IFDEF Delphi}
uses
  Dmisc;
{$ENDIF Delphi}
{$IFDEF FPC}
uses
  Dos;
{$ENDIF FPC}


(**********************************************************************
C     Benchmark Double Precision Whetstone (A001)
C
C     o This is a LONGREAL*8 version of
C       the Whetstone benchmark program.
C     o FOR-loop semantics are ANSI-66 compatible.
C     o Final measurements are to be made with all
C       WRITE statements and FORMAT sttements removed.
C
C**********************************************************************)


const
(* With loopcount NLoop=10, one million Whetstone instructions
   will be executed in each major loop.
   A major loop is executed 'II' times to increase wall-clock timing accuracy *)
   NLoopValue = 100;

{$IFDEF OS2}
function TimeNow : LongInt;
var
  Clocks : LongInt;
  rc     : ApiRet;
begin
  rc := DosQuerySysInfo(qsv_Ms_Count, qsv_Ms_Count, Clocks, SizeOf(Clocks));
  TimeNow := Clocks;
end;

{$ELSE}
function TimeNow : Int64;

var
   h,m,s,s100 : word;

begin
  gettime(h,m,s,s100);
  TimeNow := h*3600*1000+m*60*1000+s*1000+s100*10;
end;
{$ENDIF}



TYPE ARRAY4 = ARRAY [1..4] OF DOUBLE;

VAR E1                  : ARRAY4;
    T, T1, T2           : DOUBLE;
    J, K, L             : LONGINT;
    ptime, time0, time1 : DOUBLE;

PROCEDURE PA (VAR E : ARRAY4);
VAR J1 : LONGINT;
BEGIN
        J1 := 0;
        REPEAT
                E [1] := ( E [1] + E [2] + E [3] - E [4]) * T;
                E [2] := ( E [1] + E [2] - E [3] + E [4]) * T;
                E [3] := ( E [1] - E [2] + E [3] + E [4]) * T;
                E [4] := (-E [1] + E [2] + E [3] + E [4]) / T2;
                J1 := J1 + 1;
        UNTIL J1 >= 6;
END;

PROCEDURE P0;
BEGIN
        E1 [J] := E1 [K]; E1 [K] := E1 [L]; E1 [L] := E1 [J];
END;

PROCEDURE P3 (X,Y : DOUBLE; VAR Z : DOUBLE);
VAR X1, Y1 : DOUBLE;
BEGIN
        X1 := X;
        Y1 := Y;
        X1 := T * (X1 + Y1);
        Y1 := T * (X1 + Y1);
        Z := (X1 + Y1)/T2;
END;

PROCEDURE POUT (N, J, K : LONGINT; X1, X2, X3, X4 : DOUBLE);
VAR time1 : double;
BEGIN
{
        time1 := TimeNow;
        WriteLn(time1-time0:6:1,time1-ptime:6,N:6,J:6,K:6,' ',
                X1:10,' ', X2:10,'  ',X3:10,'  ',X4:10);
        ptime := time1;
}
END;

PROCEDURE DoIt;
VAR NLoop, I, II, JJ : LONGINT;
    N1, N2, N3, N4, N5, N6, N7, N8, N9, N10, N11 : LONGINT;
    X1, X2, X3, X4, X, Y, Z : DOUBLE;
BEGIN
        time0 := TimeNow;
        ptime := time0;
(* The actual benchmark starts here. *)
        T  := 0.499975;
        T1 := 0.50025;
        T2 := 2.0;
        NLoop := NLoopValue;
        II    := 400;
        FOR JJ:=1 TO II DO BEGIN
(* Establish the relative loop counts of each module. *)
                N1 := 0;
                N2 := 12 * NLoop;
                N3 := 14 * NLoop;
                N4 := 345 * NLoop;
                N5 := 0;
                N6 := 210 * NLoop;
                N7 := 32 * NLoop;
                N8 := 899 * NLoop;
                N9 := 616 * NLoop;
                N10 := 0;
                N11 := 93 * NLoop;
(* Module 1: Simple identifiers *)
                X1 := 1.0;
                X2 := -1.0;
                X3 := -1.0;
                X4 := -1.0;
                FOR I:=1 TO N1 DO BEGIN
                        X1 := (X1 + X2 + X3 - X4)*T;
                        X2 := (X1 + X2 - X3 + X4)*T;
                        X3 := (X1 - X2 + X3 + X4)*T;
                        X4 := (-X1 + X2 + X3 + X4)*T;
                END;
                IF (JJ = II) THEN BEGIN
                        POUT (N1, N1, N1, X1, X2, X3, X4);
                END;
(* Module 2: Array elements *)
                E1 [1] :=  1.0;
                E1 [2] := -1.0;
                E1 [3] := -1.0;
                E1 [4] := -1.0;
                FOR I:=1 TO N2 DO BEGIN
                        E1 [1] := (E1 [1] + E1 [2] + E1 [3] - E1 [4])*T;
                        E1 [2] := (E1 [1] + E1 [2] - E1 [3] + E1 [4])*T;
                        E1 [3] := (E1 [1] - E1 [2] + E1 [3] + E1 [4])*T;
                        E1 [4] := (-E1 [1] + E1 [2] + E1 [3] + E1 [4])*T;
                END;
                IF (JJ = II) THEN BEGIN
                        POUT (N2, N3, N2, E1 [1], E1 [2], E1 [3], E1 [4]);
                END;
(* Module 3: Array as parameter *)
                FOR I:=1 TO N3 DO BEGIN
                        PA (E1);
                END;
                IF (JJ = II) THEN BEGIN
                        POUT(N3, N2, N2, E1 [1], E1 [2], E1 [3], E1 [4]);
                END;
(* Module 4: Conditional jumps *)
                J := 1;
                FOR I:=1 TO N4 DO BEGIN
                        IF (J <> 1) THEN J := 3 ELSE J := 2;
                        IF (J <= 2) THEN J := 1 ELSE J := 0;
                        IF (J >= 1) THEN J := 0 ELSE J := 1;
                END;
                IF (JJ = II) THEN BEGIN
                        POUT (N4, J, J, X1, X2, X3, X4)
                END;
(* Module 5: Omitted; Module 6: Integer arithmetic *)
                J := 1;
                K := 2;
                L := 3;
                FOR I:=1 TO N6 DO BEGIN
                        J := J * (K-J) * (L-K);
                        K := L * K - (L-J) * K;
                        L := (L - K) * (K + J);
                        E1 [L-1] := (J + K + L);
                        E1 [K-1] := (J * K * L);
                END;
                IF (JJ = II) THEN BEGIN
                        POUT (N6, J, K, E1 [1], E1 [2], E1 [3], E1 [4]);
                END;
(* Module 7: Trigonometric functions *)
                X := 0.5;
                Y := 0.5;
                FOR I:=1 TO N7 DO BEGIN
                        X:=T*arctan(T2*sin(X)*cos(X)/(cos(X+Y)+cos(X-Y)-1.0));
                        Y:=T*arctan(T2*sin(Y)*cos(Y)/(cos(X+Y)+cos(X-Y)-1.0));
                END;
                IF (JJ = II) THEN BEGIN
                        POUT (N7, J, K, X, X, Y, Y);
                END;
(* Module 8: Procedure calls *)
                X := 1.0;
                Y := 1.0;
                Z := 1.0;
                FOR I:=1 TO N8 DO BEGIN
                        P3 (X,Y,Z);
                END;
                IF (JJ = II) THEN BEGIN
                        POUT (N8, J, K, X, Y, Z, Z);
                END;
(* Module 9: Array references *)
                J := 1;
                K := 2;
                L := 3;
                E1 [1] := 1.0;
                E1 [2] := 2.0;
                E1 [3] := 3.0;
                FOR I:=1 TO N9 DO BEGIN
                        P0;
                END;
                IF (JJ = II) THEN BEGIN
                        POUT (N9, J, K, E1 [1], E1 [2], E1 [3], E1 [4])
                END;
(* Module 10: Integer arithmetic *)
                J := 2;
                K := 3;
                FOR I:=1 TO N10 DO BEGIN
                        J := J + K;
                        K := J + K;
                        J := K - J;
                        K := K - J - J;
                END;
                IF (JJ = II) THEN BEGIN
                        POUT (N10, J, K, X1, X2, X3, X4)
                END;
(* Module 11: Standard functions *)
                X := 0.75;
                FOR I:=1 TO N11 DO BEGIN
                  X := sqrt (exp (ln (X)/T1))
                  // x:=sqrt(x);
                END;
                IF (JJ = II) THEN BEGIN
                        POUT (N11, J, K, X, X, X, X)
                END;
(* THIS IS THE END OF THE MAJOR LOOP. *)
        END;
(* Stop benchmark timing at this point. *)
        time1 := TimeNow;
(*----------------------------------------------------------------
      Performance in Whetstone KIP's per second is given by
       (100*NLoop*II)/TIME
      where TIME is in seconds.
--------------------------------------------------------------------*)
        WriteLn;
        WriteLn ('Double Whetstone KIPS ',
                 (TRUNC ((100.0 * NLoop * II) * 1000 / (time1 - time0))));
        WriteLn ('Whetstone MIPS   ',
                  1.0*NLoop*II * 1000 / (time1 - time0):12:2);
END;

BEGIN
    DoIt;
END.
