program tmoperator9;

{$MODE DELPHI}

type

  { TFoo }

  PFoo = ^TFoo;
  TFoo = record
  private
    class operator Initialize(var aFoo: TFoo);
    class operator Finalize(var aFoo: TFoo);
  end;

{ TFoo }

var
  ok_initialize: boolean = false;
  ok_finalize: boolean = false;

class operator TFoo.Initialize(var aFoo: TFoo);
begin
  ok_initialize := true;
end;

class operator TFoo.Finalize(var aFoo: TFoo);
begin
  ok_finalize := true;
end;

var
  PF: PFoo;
begin
  { init rtti test }
  New(PF);
  if not ok_initialize then
    Halt(1);
  Dispose(PF);
  if not ok_finalize then
    Halt(2);

  ok_initialize := false;
  ok_finalize := false;

  { regular rtti test }
  GetMem(PF, SizeOf(TFoo));
  InitializeArray(PF, TypeInfo(TFoo), 1);
  if not ok_initialize then
    Halt(3);
  FinalizeArray(PF, TypeInfo(TFoo), 1);
  if not ok_finalize then
    Halt(4);
  FreeMem(PF);
end. 