{ %SKIPTARGET=macos }
{ On macos, PPCAsm chokes on this and crashes}

type
  de10_eqn_vector = array [1..10] of double;
  de10_func = function : double;
  de10func = de10_func;
  DE10_PHI_ARRAY = array[1..10] of double;
  DE10phiarray = DE10_PHI_ARRAY;
  de10eqnvec = de10_eqn_vector;
  de10_12_vector = array [10..12] of double;
  de10_13_vector = array [10..13] of double;
  de1012vec = de10_12_vector;
  de1013vec = de10_13_vector;

PROCEDURE Step10( VAR X : double; VAR Y :
                     DE10_EQN_VECTOR; F10 : DE10_FUNC; VAR NEQN : INTEGER; VAR H : double; VAR
                     EPS : double; VAR WT : DE10_EQN_VECTOR; VAR START : BOOLEAN; VAR HOLD :
                     double; VAR K : INTEGER; VAR KOLD : INTEGER; VAR CRASH : BOOLEAN; VAR PHI :
                     DE10_PHI_ARRAY; VAR P : DE10_EQN_VECTOR; VAR YP : DE10_EQN_VECTOR;
                     VAR PSI : DE10_12_VECTOR; VAR ALPHA : DE10_12_VECTOR; VAR BETA :
                     DE10_12_VECTOR; VAR SIG : DE10_13_VECTOR; VAR V : DE10_12_VECTOR; VAR W
                     : DE10_12_VECTOR; VAR G : DE10_13_VECTOR; VAR PHASE1 : BOOLEAN; VAR NS :
                     INTEGER; VAR NORND : BOOLEAN );
  begin
  end;

PROCEDURE Step11( VAR X : double; VAR Y : DE10EQNVEC; F10 : DE10FUNC; VAR
                     NEQN : INTEGER; VAR H : double; VAR EPS : double; VAR WT : DE10EQNVEC; VAR
                     START : BOOLEAN; VAR HOLD : double; VAR K : INTEGER; VAR KOLD : INTEGER;
                     VAR CRASH : BOOLEAN; VAR PHI : DE10PHIARRAY; VAR P : DE10EQNVEC; VAR YP
                     : DE10EQNVEC; VAR PSI : DE1012VEC; VAR ALPHA : DE1012VEC; VAR BETA :
                     DE1012VEC; VAR SIG : DE1013VEC; VAR V : DE1012VEC; VAR W : DE1012VEC; VAR
                     G : DE1013VEC; VAR PHASE1 : BOOLEAN; VAR NS : INTEGER; VAR NORND :
                     BOOLEAN );

  begin
  end;



begin
end.
