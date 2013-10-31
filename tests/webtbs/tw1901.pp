{ %version=1.1 }

{$ifdef fpc}
{$MODE DELPHI}
{$endif}

{ Range and overflow checks need to be off }

{$Q-}
{$R-}

const Inf=1/0;
      NaN=0/0;
      MinusInf=-Inf;

function make_str( tail: string ): string;
var
    float: extended;
    test: string;
    n_test, n_tail: integer;
begin
    float := 0;
    str( float, test );
    n_test := length( test );
    n_tail := length( tail );
    if ( n_test <= n_tail ) then
        make_str := tail
    else
      begin
        fillchar( test[ 1 ], n_test - n_tail, ' ' );
        move( tail[ 1 ], test[ n_test - n_tail + 1 ], n_tail );
        make_str := test;
      end;
end;

var
  s : string;
  error : boolean;
  s1, s2, s3 : string;
begin
  s1 := make_str( '+Inf' );
  s2 := make_str( 'Nan' );
  s3 := make_str( '-Inf' );
  error:=false;
  str(Inf,s);
  writeln('Inf: "',s,'"');
  if s<>s1 then
   error:=true;
  str(NaN,s);
  writeln('Nan: "',s,'"');
  if s<>s2 then
   error:=true;
  str(MinusInf,s);
  writeln('MinusInf: "',s,'"');
  if s<>s3 then
   error:=true;
  if error then
   begin
     writeln('ERROR!');
     halt(1);
   end;
end.
