{ Source provided for Free Pascal Bug Report 4007 }
{ Submitted by "Torsten Kildal" on  2005-05-23 }
{ e-mail: kildal@gmx.de }
PROGRAM H;
{$IFDEF FPC} {$MODE TP} {$ENDIF}

VAR
  Handler: function: boolean;

  function AHandler: boolean; far;
  begin
    AHandler:=true;
  end;

begin
  Handler:= AHandler;

  {here FPC differs from BP ->BUG}
  if Handler then writeln('1');
  {BP7 : '1'}
  {1010: '1'}
  {19x : Boolean expression expected, but got "<procedure variable type of function:Boolean;Register>"}
  {200 : Boolean expression expected, but got "<procedure variable type of function:Boolean;Register>"}

  {the same with "=true" shows no difference -> conflict to '1'}
  if Handler=true then writeln('2');
  {BP7 : '2'}
  {1010: '2'}
  {19x : '2'}
  {200 : '2'}

  {here FPC differs from BP again -> BUG}
  if Handler() then writeln('3');
  {BP7 : THEN expected}
  {1010: '3'}
  {19x : '3'}
  {200 : '3'}

{$ifdef nok}
  {this both BP and FPC don't like -> ok}
  if Handler=NIL then writeln('4');
  {BP7 : Type Mismatch}
  {1010: Operator is not overloaded}
  {19x : Incompatible types: got "Boolean" expected "LongInt"}
  {200 : Incompatible types: got "Boolean" expected "LongInt"}
{$endif nok}

  {the rest is accepted by both BP and FPC -> ok}
  if @Handler<>NIL     then writeln('5');
  if assigned(Handler) then writeln('6');

end.

(*
  "BP7"  = Borland Pascal 7.0       -> 1,2,    5,6
  "1010" = FPC 1.0.10               -> 1,2,3,  5,6
  "19x"  = FPC 1.9.4 / 1.9.6        ->   2,3,  5,6
  "200"  = FPC 2.0.0                ->   2,3,  5,6
*)
