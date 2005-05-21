{ Source provided for Free Pascal Bug Report 2268 }
{ Submitted by "marco" on  2002-12-19 }
{ e-mail: marco@freepascal.org }
{$ifdef fpc}{$mode TP}{$endif}

function P1:longint; begin end;
function P2:longint; begin end;
function P3:longint; begin end;
function Help:longint; begin end;

type Fn = function : longint ;
const PArr : array [0..3] of Fn = (Help, P1, P2, P3) ;

procedure bla(const i:longint); begin end;

begin
 bla(PArr[Paramcount]);
end.
