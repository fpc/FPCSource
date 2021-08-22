{ %cpu=i8086,i386 }

{$ifdef fpc}
{$mode tp}
{$endif}

procedure FillWord(var Dest; Count: Word; Data: Word);
begin
{$ifdef CPU386}
  inline(
    $8b/$7d/<Dest/        (* MOV   EDI,Dest  *)
    $0f/$b7/$4d/<Count/   (* MOVZX ECX,Count *)
    $66/$8b/$45/<Data/    (* MOV   AX,Data   *)
    $fc/                  (* CLD             *)
    $f3/$66/$ab);         (* REP   STOSW     *)
{$else}
  inline(
    $C4/$7E/<Dest/        (* LES   DI,Dest[BP] *)
    $8B/$4E/<Count/       (* MOV   CX,Count[BP]*)
    $8B/$46/<Data/        (* MOV   AX,Data[BP] *)
    $FC/                  (* CLD               *)
    $F3/$AB);             (* REP   STOSW       *)
{$endif}
end;

var
  arr: array[1..10] of word;
  i: integer;
begin
{$if sizeof(pointer)<4}
  writeln('Skipping.');
  Halt(0);
{$endif}
  FillChar(arr,sizeof(arr),$aa);
  FillWord(arr,sizeof(arr) div 2,$55);
  for i:=1 to 10 do
    if arr[i]<>$55 then
      begin
        writeln('Wrong value: ', arr[i]);
        Halt(1);
      end;
  writeln('OK.');
end.
