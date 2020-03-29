{ %cpu=i8086 }
{ test applies only to these memory models: }
{$if defined(FPC_MM_MEDIUM) or defined(FPC_MM_LARGE) or defined(FPC_MM_HUGE)}

{$mode TP}

{$F-}
{$hugecode off}

{ should be near, since we are in $F- mode }
procedure myproc;
begin
  Writeln('myproc');
end;

procedure mynearproc; near;
begin
  Writeln('mynearproc');
end;

procedure myfarproc; far;
begin
  Writeln('myfarproc');
end;

type
  TMyObject = object
    procedure RegularMethod;
  end;

procedure TMyObject.RegularMethod;
begin
  Writeln('TMyObject.RegularMethod');
end;

procedure Error;
begin
  Writeln('Error!');
  Halt(1);
end;

var
  prcn: Procedure; near;
  prc: Procedure;
  prcf: Procedure; far;
  ptr_prcn: Word absolute prcn;
  ptr_prc: FarPointer absolute prc;
  ptr_prcf: FarPointer absolute prcf;
  w: Word;
  P, PA: CodePointer;
begin
  prcn := myproc;
  prcn;
  prcn := mynearproc;
  prcn;
  prc := myfarproc;
  prc;
  prcf := myfarproc;
  prcf;

  prcn := myproc;
  w := Ofs(myproc);
  P := @myproc;
  PA := Addr(myproc);
  if ptr_prcn <> w then
    Error;
  if P <> PA then
    Error;
  if Ofs(P^) <> w then
    Error;
  if Seg(P^) <> Seg(myproc) then
    Error;

  prcn := mynearproc;
  w := Ofs(mynearproc);
  P := @mynearproc;
  PA := Addr(mynearproc);
  if ptr_prcn <> w then
    Error;
  if P <> PA then
    Error;
  if Ofs(P^) <> w then
    Error;
  if Seg(P^) <> Seg(mynearproc) then
    Error;

  prcf := myfarproc;
  w := Ofs(myfarproc);
  P := @myfarproc;
  PA := Addr(myfarproc);
  if ptr_prcf <> P then
    Error;
  if P <> PA then
    Error;
  if Ofs(P^) <> w then
    Error;
  if Seg(P^) <> Seg(myfarproc) then
    Error;

  P := @TMyObject.RegularMethod;
  PA := Addr(TMyObject.RegularMethod);
  w := Ofs(TMyObject.RegularMethod);
  if P <> PA then
    Error;
  if Ofs(P^) <> w then
    Error;
  if Seg(P^) <> Seg(TMyObject.RegularMethod) then
    Error;

  Writeln('Ok!');
end.
{$else}
begin
  { silently succeed in the other memory models }
end.
{$endif}
