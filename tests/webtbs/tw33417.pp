type
  tflags = bitpacked record // Flags
    bit0,bit1,bit2,bit3,bit4,bit5,bit6,bit7 : boolean;
  end;

var
  gflags : tflags;

procedure p;

var
  flags : tflags;
begin
  flags:=gflags;
  if flags.bit5 then
    halt(1);
  if gflags.bit5 then
    halt(1);
  if not flags.bit6 then
    halt(1);
  if not gflags.bit6 then
    halt(1);
end;

begin
  gflags.bit4:=false;
  gflags.bit5:=false;
  gflags.bit6:=true;
  p;
  writeln('ok');
end.
