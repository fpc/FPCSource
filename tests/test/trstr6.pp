{ from GPC test suite }

program rstr1(Output);
type ii = integer;
     tip = ^ii;
var ipv1, ipv2, ipv3 : tip;
    s : string[20];

function ip1: tip;
  var tmp : tip;
begin
  s := 'dead beef';
  tmp := ipv2;
  ipv2 := ipv3;
  ipv3 := tmp;
  ip1 := ipv1;
end;
begin
    s:='666 123';
    new(ipv1);
    new(ipv2);
    new(ipv3);
    ipv2^ := 155;
    readstr(s, ip1^, ipv2^);
    if (ipv1^ = 666) and (ipv2^ = 123) and (ipv3^ = 155) then
      writeln('OK')
    else
      halt(1);
end. 
