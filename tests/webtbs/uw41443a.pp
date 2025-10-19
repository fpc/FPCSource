unit uw41443a;
{$mode objfpc}

interface

uses
  uw41443b;

procedure uw41443a_proc2;

implementation

type
  tdummy_uw41443a = -1..7;

const
  uw41443a_counter : tdummy_uw41443a = 0;

procedure uw41443a_proc1;{$ifndef DISABLE_PROC1_INLINE} inline;{$endif}
begin
  inc(uw41443a_counter);
end;

procedure uw41443a_proc2; inline;
begin
  uw41443a_proc1;
end;

end.
