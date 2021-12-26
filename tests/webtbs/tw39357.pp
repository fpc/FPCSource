{ %OPT=-O- -O2 }

function get_sign(d: double): Integer;
  var
    p: pbyte;
  begin
    get_sign:=1;
    p:=pbyte(@d);
{$ifdef FPUARM_HAS_FPA}
    inc(p,4);
{$else}
{$ifdef FPC_LITTLE_ENDIAN}
    inc(p,sizeof(d)-1);
{$endif}
{$endif}
    if (p^ and $80)<>0 then
      get_sign:=-1;
  end;

const
	NegInfinity: single = -1.0 / 0.0;
var
    zero : Double;
begin
    zero:=0.0;

    if get_sign(1.0)<>1 then
      halt(1);

	writeln(-zero);
    if get_sign(-zero)<>-1 then
      halt(1);

	writeln(1.0 / (-1.0 / 0.0));
    if get_sign(1.0 / (-1.0 / 0.0))<>-1 then
      halt(1);

	writeln(1.0 / NegInfinity);
    if get_sign(1.0 / NegInfinity)<>-1 then
      halt(1);

    writeln('ok');
end.
