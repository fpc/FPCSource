{ %norun }

{$MODE objfpc}
unit tw8633;
interface

function dorm2r_(var side: Char; var trans: Char; var m: Integer; var n: Integer; var k: Integer; var a: Double; var lda: Integer; var tau: Double; var c__: Double; var ldc: Integer; var work: Double; var info: Integer; side_len: Integer; trans_len: Integer): Integer; cdecl; external;
function dormqr_(var side: Char; var trans: Char; var m: Integer; var n: Integer; var k: Integer; var a: Double; var lda: Integer; var tau: Double; var c__: Double; var ldc: Integer; var work: Double; var lwork: Integer; var info: Integer; side_len: Integer; trans_len: Integer): Integer; cdecl;

implementation

uses SysUtils, Math;

function ILAENV(ispec: Integer; name__: string; opts: string;
                 n1: Integer; n2: Integer; n3: Integer; n4: Integer): Integer;
begin
  Result := 0; 
end;

function dormqr_(var side: Char; var trans: Char; var m: Integer; var n: Integer; var k: Integer; var a: Double; var lda: Integer; var tau: Double; var c__: Double; var ldc: Integer; var work: Double; var lwork: Integer; var info: Integer; side_len: Integer; trans_len: Integer): Integer; cdecl;
var
  iinfo, iws, ldwork,
  lwkopt, nb, nbmin, nw: Integer;
  T: array [1..65*64] of Double;
begin

      NBMIN := 2;
      LDWORK := NW;
      IF ( NB > 1 ) and ( NB < K ) THEN BEGIN
         IWS := NW*NB;
         IF LWORK < IWS THEN BEGIN
            NB := LWORK div LDWORK;
            NBMIN := MAX( 2, ILAENV( 2, 'DORMQR', SIDE + TRANS, M, N, K,-1 ) );
         END;
      END ELSE
         IWS := NW;

      IF( NB < NBMIN ) or  ( NB >= K ) THEN
       dorm2r_( SIDE, TRANS, M, N, K, A, LDA, TAU, c__, LDC, WORK, IINFO, side_len, trans_len );
      WORK := LWKOPT;
end;

end.



