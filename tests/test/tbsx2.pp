{$mode objfpc}

program tbsx2;

function test_byte: boolean;
var
  f,r: byte;
begin
  f:=BsfByte($07);
  if f<>0 then
  begin
    writeln('BsfByte($07) returned ',f,', should be 0');
    exit(false);
  end;
  f:=BsrByte($01);
  if f<>0 then
  begin
    writeln('BsrByte($01) returned ',f,', should be 0');
    exit(false);
  end;
  f:=BsfByte($0E);
  if f<>1 then
  begin
    writeln('BsfByte($0E) returned ',f,', should be 1');
    exit(false);
  end;
  f:=BsrByte($03);
  if f<>1 then
  begin
    writeln('BsrByte($03) returned ',f,', should be 1');
    exit(false);
  end;
  f:=BsfByte($14);
  if f<>2 then
  begin
    writeln('BsfByte($14) returned ',f,', should be 2');
    exit(false);
  end;
  f:=BsrByte($06);
  if f<>2 then
  begin
    writeln('BsrByte($06) returned ',f,', should be 2');
    exit(false);
  end;
  f:=BsfByte($28);
  if f<>3 then
  begin
    writeln('BsfByte($28) returned ',f,', should be 3');
    exit(false);
  end;
  f:=BsrByte($0B);
  if f<>3 then
  begin
    writeln('BsrByte($0B) returned ',f,', should be 3');
    exit(false);
  end;
  f:=BsfByte($30);
  if f<>4 then
  begin
    writeln('BsfByte($30) returned ',f,', should be 4');
    exit(false);
  end;
  f:=BsrByte($14);
  if f<>4 then
  begin
    writeln('BsrByte($14) returned ',f,', should be 4');
    exit(false);
  end;
  f:=BsfByte($60);
  if f<>5 then
  begin
    writeln('BsfByte($60) returned ',f,', should be 5');
    exit(false);
  end;
  f:=BsrByte($25);
  if f<>5 then
  begin
    writeln('BsrByte($25) returned ',f,', should be 5');
    exit(false);
  end;
  f:=BsfByte($40);
  if f<>6 then
  begin
    writeln('BsfByte($40) returned ',f,', should be 6');
    exit(false);
  end;
  f:=BsrByte($46);
  if f<>6 then
  begin
    writeln('BsrByte($46) returned ',f,', should be 6');
    exit(false);
  end;
  f:=BsfByte($80);
  if f<>7 then
  begin
    writeln('BsfByte($80) returned ',f,', should be 7');
    exit(false);
  end;
  f:=BsrByte($87);
  if f<>7 then
  begin
    writeln('BsrByte($87) returned ',f,', should be 7');
    exit(false);
  end;

  f:=BsfByte(0);
  if (f<>$ff) then
  begin
    writeln('BsfByte(0) returned ',f,', should be ',$ff);
    exit(false);
  end;
  r:=BsrByte(0);
  if r<>$ff then
  begin
    writeln('BsrByte(0) returned ',r,', should be ',$ff);
    exit(false);
  end;
  result:=true;
end;

function test_word: boolean;
var
  f,r: integer;
begin
  f:=BsfWord(15);
  if f<>0 then
  begin
    writeln('BsfWord(15) returned ',f,', should be 0');
    exit(false);
  end;
  f:=BsrWord(1);
  if f<>0 then
  begin
    writeln('BsrWord(1) returned ',f,', should be 0');
    exit(false);
  end;
  f:=BsfWord(30);
  if f<>1 then
  begin
    writeln('BsfWord(30) returned ',f,', should be 1');
    exit(false);
  end;
  f:=BsrWord(3);
  if f<>1 then
  begin
    writeln('BsrWord(3) returned ',f,', should be 1');
    exit(false);
  end;
  f:=BsfWord(52);
  if f<>2 then
  begin
    writeln('BsfWord(52) returned ',f,', should be 2');
    exit(false);
  end;
  f:=BsrWord(6);
  if f<>2 then
  begin
    writeln('BsrWord(6) returned ',f,', should be 2');
    exit(false);
  end;
  f:=BsfWord(104);
  if f<>3 then
  begin
    writeln('BsfWord(104) returned ',f,', should be 3');
    exit(false);
  end;
  f:=BsrWord(11);
  if f<>3 then
  begin
    writeln('BsrWord(11) returned ',f,', should be 3');
    exit(false);
  end;
  f:=BsfWord(176);
  if f<>4 then
  begin
    writeln('BsfWord(176) returned ',f,', should be 4');
    exit(false);
  end;
  f:=BsrWord(20);
  if f<>4 then
  begin
    writeln('BsrWord(20) returned ',f,', should be 4');
    exit(false);
  end;
  f:=BsfWord(352);
  if f<>5 then
  begin
    writeln('BsfWord(352) returned ',f,', should be 5');
    exit(false);
  end;
  f:=BsrWord(37);
  if f<>5 then
  begin
    writeln('BsrWord(37) returned ',f,', should be 5');
    exit(false);
  end;
  f:=BsfWord(576);
  if f<>6 then
  begin
    writeln('BsfWord(576) returned ',f,', should be 6');
    exit(false);
  end;
  f:=BsrWord(70);
  if f<>6 then
  begin
    writeln('BsrWord(70) returned ',f,', should be 6');
    exit(false);
  end;
  f:=BsfWord(1152);
  if f<>7 then
  begin
    writeln('BsfWord(1152) returned ',f,', should be 7');
    exit(false);
  end;
  f:=BsrWord(135);
  if f<>7 then
  begin
    writeln('BsrWord(135) returned ',f,', should be 7');
    exit(false);
  end;
  f:=BsfWord(1792);
  if f<>8 then
  begin
    writeln('BsfWord(1792) returned ',f,', should be 8');
    exit(false);
  end;
  f:=BsrWord(264);
  if f<>8 then
  begin
    writeln('BsrWord(264) returned ',f,', should be 8');
    exit(false);
  end;
  f:=BsfWord(3584);
  if f<>9 then
  begin
    writeln('BsfWord(3584) returned ',f,', should be 9');
    exit(false);
  end;
  f:=BsrWord(521);
  if f<>9 then
  begin
    writeln('BsrWord(521) returned ',f,', should be 9');
    exit(false);
  end;
  f:=BsfWord(5120);
  if f<>10 then
  begin
    writeln('BsfWord(5120) returned ',f,', should be 10');
    exit(false);
  end;
  f:=BsrWord(1034);
  if f<>10 then
  begin
    writeln('BsrWord(1034) returned ',f,', should be 10');
    exit(false);
  end;
  f:=BsfWord(10240);
  if f<>11 then
  begin
    writeln('BsfWord(10240) returned ',f,', should be 11');
    exit(false);
  end;
  f:=BsrWord(2059);
  if f<>11 then
  begin
    writeln('BsrWord(2059) returned ',f,', should be 11');
    exit(false);
  end;
  f:=BsfWord(12288);
  if f<>12 then
  begin
    writeln('BsfWord(12288) returned ',f,', should be 12');
    exit(false);
  end;
  f:=BsrWord(4108);
  if f<>12 then
  begin
    writeln('BsrWord(4108) returned ',f,', should be 12');
    exit(false);
  end;
  f:=BsfWord(24576);
  if f<>13 then
  begin
    writeln('BsfWord(24576) returned ',f,', should be 13');
    exit(false);
  end;
  f:=BsrWord(8205);
  if f<>13 then
  begin
    writeln('BsrWord(8205) returned ',f,', should be 13');
    exit(false);
  end;
  f:=BsfWord(16384);
  if f<>14 then
  begin
    writeln('BsfWord(16384) returned ',f,', should be 14');
    exit(false);
  end;
  f:=BsrWord(16398);
  if f<>14 then
  begin
    writeln('BsrWord(16398) returned ',f,', should be 14');
    exit(false);
  end;
  f:=BsfWord(32768);
  if f<>15 then
  begin
    writeln('BsfWord(32768) returned ',f,', should be 15');
    exit(false);
  end;
  f:=BsrWord(32783);
  if f<>15 then
  begin
    writeln('BsrWord(32783) returned ',f,', should be 15');
    exit(false);
  end;

  f:=BsfWord(0);
  if (f<>$ff) then
  begin
    writeln('BsfWord(0) returned ',f,', should be ',$ff);
    exit(false);
  end;
  r:=BsrWord(0);
  if r<>$ff then
  begin
    writeln('BsrWord(0) returned ',r,', should be ',$ff);
    exit(false);
  end;
  result:=true;
end;

function test_dword: boolean;
var
  f,r: integer;
begin
  f:=BsfDWord(31);
  if f<>0 then
  begin
    writeln('BsfDWord(31) returned ',f,', should be 0');
    exit(false);
  end;
  f:=BsrDWord(1);
  if f<>0 then
  begin
    writeln('BsrDWord(1) returned ',f,', should be 0');
    exit(false);
  end;
  f:=BsfDWord(62);
  if f<>1 then
  begin
    writeln('BsfDWord(62) returned ',f,', should be 1');
    exit(false);
  end;
  f:=BsrDWord(3);
  if f<>1 then
  begin
    writeln('BsrDWord(3) returned ',f,', should be 1');
    exit(false);
  end;
  f:=BsfDWord(116);
  if f<>2 then
  begin
    writeln('BsfDWord(116) returned ',f,', should be 2');
    exit(false);
  end;
  f:=BsrDWord(6);
  if f<>2 then
  begin
    writeln('BsrDWord(6) returned ',f,', should be 2');
    exit(false);
  end;
  f:=BsfDWord(232);
  if f<>3 then
  begin
    writeln('BsfDWord(232) returned ',f,', should be 3');
    exit(false);
  end;
  f:=BsrDWord(11);
  if f<>3 then
  begin
    writeln('BsrDWord(11) returned ',f,', should be 3');
    exit(false);
  end;
  f:=BsfDWord(432);
  if f<>4 then
  begin
    writeln('BsfDWord(432) returned ',f,', should be 4');
    exit(false);
  end;
  f:=BsrDWord(20);
  if f<>4 then
  begin
    writeln('BsrDWord(20) returned ',f,', should be 4');
    exit(false);
  end;
  f:=BsfDWord(864);
  if f<>5 then
  begin
    writeln('BsfDWord(864) returned ',f,', should be 5');
    exit(false);
  end;
  f:=BsrDWord(37);
  if f<>5 then
  begin
    writeln('BsrDWord(37) returned ',f,', should be 5');
    exit(false);
  end;
  f:=BsfDWord(1600);
  if f<>6 then
  begin
    writeln('BsfDWord(1600) returned ',f,', should be 6');
    exit(false);
  end;
  f:=BsrDWord(70);
  if f<>6 then
  begin
    writeln('BsrDWord(70) returned ',f,', should be 6');
    exit(false);
  end;
  f:=BsfDWord(3200);
  if f<>7 then
  begin
    writeln('BsfDWord(3200) returned ',f,', should be 7');
    exit(false);
  end;
  f:=BsrDWord(135);
  if f<>7 then
  begin
    writeln('BsrDWord(135) returned ',f,', should be 7');
    exit(false);
  end;
  f:=BsfDWord(5888);
  if f<>8 then
  begin
    writeln('BsfDWord(5888) returned ',f,', should be 8');
    exit(false);
  end;
  f:=BsrDWord(264);
  if f<>8 then
  begin
    writeln('BsrDWord(264) returned ',f,', should be 8');
    exit(false);
  end;
  f:=BsfDWord(11776);
  if f<>9 then
  begin
    writeln('BsfDWord(11776) returned ',f,', should be 9');
    exit(false);
  end;
  f:=BsrDWord(521);
  if f<>9 then
  begin
    writeln('BsrDWord(521) returned ',f,', should be 9');
    exit(false);
  end;
  f:=BsfDWord(21504);
  if f<>10 then
  begin
    writeln('BsfDWord(21504) returned ',f,', should be 10');
    exit(false);
  end;
  f:=BsrDWord(1034);
  if f<>10 then
  begin
    writeln('BsrDWord(1034) returned ',f,', should be 10');
    exit(false);
  end;
  f:=BsfDWord(43008);
  if f<>11 then
  begin
    writeln('BsfDWord(43008) returned ',f,', should be 11');
    exit(false);
  end;
  f:=BsrDWord(2059);
  if f<>11 then
  begin
    writeln('BsrDWord(2059) returned ',f,', should be 11');
    exit(false);
  end;
  f:=BsfDWord(77824);
  if f<>12 then
  begin
    writeln('BsfDWord(77824) returned ',f,', should be 12');
    exit(false);
  end;
  f:=BsrDWord(4108);
  if f<>12 then
  begin
    writeln('BsrDWord(4108) returned ',f,', should be 12');
    exit(false);
  end;
  f:=BsfDWord(155648);
  if f<>13 then
  begin
    writeln('BsfDWord(155648) returned ',f,', should be 13');
    exit(false);
  end;
  f:=BsrDWord(8205);
  if f<>13 then
  begin
    writeln('BsrDWord(8205) returned ',f,', should be 13');
    exit(false);
  end;
  f:=BsfDWord(278528);
  if f<>14 then
  begin
    writeln('BsfDWord(278528) returned ',f,', should be 14');
    exit(false);
  end;
  f:=BsrDWord(16398);
  if f<>14 then
  begin
    writeln('BsrDWord(16398) returned ',f,', should be 14');
    exit(false);
  end;
  f:=BsfDWord(557056);
  if f<>15 then
  begin
    writeln('BsfDWord(557056) returned ',f,', should be 15');
    exit(false);
  end;
  f:=BsrDWord(32783);
  if f<>15 then
  begin
    writeln('BsrDWord(32783) returned ',f,', should be 15');
    exit(false);
  end;
  f:=BsfDWord(983040);
  if f<>16 then
  begin
    writeln('BsfDWord(983040) returned ',f,', should be 16');
    exit(false);
  end;
  f:=BsrDWord(65552);
  if f<>16 then
  begin
    writeln('BsrDWord(65552) returned ',f,', should be 16');
    exit(false);
  end;
  f:=BsfDWord(1966080);
  if f<>17 then
  begin
    writeln('BsfDWord(1966080) returned ',f,', should be 17');
    exit(false);
  end;
  f:=BsrDWord(131089);
  if f<>17 then
  begin
    writeln('BsrDWord(131089) returned ',f,', should be 17');
    exit(false);
  end;
  f:=BsfDWord(3407872);
  if f<>18 then
  begin
    writeln('BsfDWord(3407872) returned ',f,', should be 18');
    exit(false);
  end;
  f:=BsrDWord(262162);
  if f<>18 then
  begin
    writeln('BsrDWord(262162) returned ',f,', should be 18');
    exit(false);
  end;
  f:=BsfDWord(6815744);
  if f<>19 then
  begin
    writeln('BsfDWord(6815744) returned ',f,', should be 19');
    exit(false);
  end;
  f:=BsrDWord(524307);
  if f<>19 then
  begin
    writeln('BsrDWord(524307) returned ',f,', should be 19');
    exit(false);
  end;
  f:=BsfDWord(11534336);
  if f<>20 then
  begin
    writeln('BsfDWord(11534336) returned ',f,', should be 20');
    exit(false);
  end;
  f:=BsrDWord(1048596);
  if f<>20 then
  begin
    writeln('BsrDWord(1048596) returned ',f,', should be 20');
    exit(false);
  end;
  f:=BsfDWord(23068672);
  if f<>21 then
  begin
    writeln('BsfDWord(23068672) returned ',f,', should be 21');
    exit(false);
  end;
  f:=BsrDWord(2097173);
  if f<>21 then
  begin
    writeln('BsrDWord(2097173) returned ',f,', should be 21');
    exit(false);
  end;
  f:=BsfDWord(37748736);
  if f<>22 then
  begin
    writeln('BsfDWord(37748736) returned ',f,', should be 22');
    exit(false);
  end;
  f:=BsrDWord(4194326);
  if f<>22 then
  begin
    writeln('BsrDWord(4194326) returned ',f,', should be 22');
    exit(false);
  end;
  f:=BsfDWord(75497472);
  if f<>23 then
  begin
    writeln('BsfDWord(75497472) returned ',f,', should be 23');
    exit(false);
  end;
  f:=BsrDWord(8388631);
  if f<>23 then
  begin
    writeln('BsrDWord(8388631) returned ',f,', should be 23');
    exit(false);
  end;
  f:=BsfDWord(117440512);
  if f<>24 then
  begin
    writeln('BsfDWord(117440512) returned ',f,', should be 24');
    exit(false);
  end;
  f:=BsrDWord(16777240);
  if f<>24 then
  begin
    writeln('BsrDWord(16777240) returned ',f,', should be 24');
    exit(false);
  end;
  f:=BsfDWord(234881024);
  if f<>25 then
  begin
    writeln('BsfDWord(234881024) returned ',f,', should be 25');
    exit(false);
  end;
  f:=BsrDWord(33554457);
  if f<>25 then
  begin
    writeln('BsrDWord(33554457) returned ',f,', should be 25');
    exit(false);
  end;
  f:=BsfDWord(335544320);
  if f<>26 then
  begin
    writeln('BsfDWord(335544320) returned ',f,', should be 26');
    exit(false);
  end;
  f:=BsrDWord(67108890);
  if f<>26 then
  begin
    writeln('BsrDWord(67108890) returned ',f,', should be 26');
    exit(false);
  end;
  f:=BsfDWord(671088640);
  if f<>27 then
  begin
    writeln('BsfDWord(671088640) returned ',f,', should be 27');
    exit(false);
  end;
  f:=BsrDWord(134217755);
  if f<>27 then
  begin
    writeln('BsrDWord(134217755) returned ',f,', should be 27');
    exit(false);
  end;
  f:=BsfDWord(805306368);
  if f<>28 then
  begin
    writeln('BsfDWord(805306368) returned ',f,', should be 28');
    exit(false);
  end;
  f:=BsrDWord(268435484);
  if f<>28 then
  begin
    writeln('BsrDWord(268435484) returned ',f,', should be 28');
    exit(false);
  end;
  f:=BsfDWord(1610612736);
  if f<>29 then
  begin
    writeln('BsfDWord(1610612736) returned ',f,', should be 29');
    exit(false);
  end;
  f:=BsrDWord(536870941);
  if f<>29 then
  begin
    writeln('BsrDWord(536870941) returned ',f,', should be 29');
    exit(false);
  end;
  f:=BsfDWord(1073741824);
  if f<>30 then
  begin
    writeln('BsfDWord(1073741824) returned ',f,', should be 30');
    exit(false);
  end;
  f:=BsrDWord(1073741854);
  if f<>30 then
  begin
    writeln('BsrDWord(1073741854) returned ',f,', should be 30');
    exit(false);
  end;
  f:=BsfDWord(2147483648);
  if f<>31 then
  begin
    writeln('BsfDWord(2147483648) returned ',f,', should be 31');
    exit(false);
  end;
  f:=BsrDWord(2147483679);
  if f<>31 then
  begin
    writeln('BsrDWord(2147483679) returned ',f,', should be 31');
    exit(false);
  end;

  f:=BsfDWord(0);
  if (f<>$ff) then
  begin
    writeln('BsfDWord(0) returned ',f,', should be ',$ff);
    exit(false);
  end;
  r:=BsrDWord(0);
  if r<>$ff then
  begin
    writeln('BsrDWord(0) returned ',r,', should be ',$ff);
    exit(false);
  end;
  result:=true;
end;

function test_qword: boolean;
var
  f, r: integer;
begin
  f:=BsfQWord(63);
  if f<>0 then
  begin
    writeln('BsfQWord(63) returned ',f,', should be 0');
    exit(false);
  end;
  f:=BsrQWord(1);
  if f<>0 then
  begin
    writeln('BsrQWord(1) returned ',f,', should be 0');
    exit(false);
  end;
  f:=BsfQWord(126);
  if f<>1 then
  begin
    writeln('BsfQWord(126) returned ',f,', should be 1');
    exit(false);
  end;
  f:=BsrQWord(3);
  if f<>1 then
  begin
    writeln('BsrQWord(3) returned ',f,', should be 1');
    exit(false);
  end;
  f:=BsfQWord(244);
  if f<>2 then
  begin
    writeln('BsfQWord(244) returned ',f,', should be 2');
    exit(false);
  end;
  f:=BsrQWord(6);
  if f<>2 then
  begin
    writeln('BsrQWord(6) returned ',f,', should be 2');
    exit(false);
  end;
  f:=BsfQWord(488);
  if f<>3 then
  begin
    writeln('BsfQWord(488) returned ',f,', should be 3');
    exit(false);
  end;
  f:=BsrQWord(11);
  if f<>3 then
  begin
    writeln('BsrQWord(11) returned ',f,', should be 3');
    exit(false);
  end;
  f:=BsfQWord(944);
  if f<>4 then
  begin
    writeln('BsfQWord(944) returned ',f,', should be 4');
    exit(false);
  end;
  f:=BsrQWord(20);
  if f<>4 then
  begin
    writeln('BsrQWord(20) returned ',f,', should be 4');
    exit(false);
  end;
  f:=BsfQWord(1888);
  if f<>5 then
  begin
    writeln('BsfQWord(1888) returned ',f,', should be 5');
    exit(false);
  end;
  f:=BsrQWord(37);
  if f<>5 then
  begin
    writeln('BsrQWord(37) returned ',f,', should be 5');
    exit(false);
  end;
  f:=BsfQWord(3648);
  if f<>6 then
  begin
    writeln('BsfQWord(3648) returned ',f,', should be 6');
    exit(false);
  end;
  f:=BsrQWord(70);
  if f<>6 then
  begin
    writeln('BsrQWord(70) returned ',f,', should be 6');
    exit(false);
  end;
  f:=BsfQWord(7296);
  if f<>7 then
  begin
    writeln('BsfQWord(7296) returned ',f,', should be 7');
    exit(false);
  end;
  f:=BsrQWord(135);
  if f<>7 then
  begin
    writeln('BsrQWord(135) returned ',f,', should be 7');
    exit(false);
  end;
  f:=BsfQWord(14080);
  if f<>8 then
  begin
    writeln('BsfQWord(14080) returned ',f,', should be 8');
    exit(false);
  end;
  f:=BsrQWord(264);
  if f<>8 then
  begin
    writeln('BsrQWord(264) returned ',f,', should be 8');
    exit(false);
  end;
  f:=BsfQWord(28160);
  if f<>9 then
  begin
    writeln('BsfQWord(28160) returned ',f,', should be 9');
    exit(false);
  end;
  f:=BsrQWord(521);
  if f<>9 then
  begin
    writeln('BsrQWord(521) returned ',f,', should be 9');
    exit(false);
  end;
  f:=BsfQWord(54272);
  if f<>10 then
  begin
    writeln('BsfQWord(54272) returned ',f,', should be 10');
    exit(false);
  end;
  f:=BsrQWord(1034);
  if f<>10 then
  begin
    writeln('BsrQWord(1034) returned ',f,', should be 10');
    exit(false);
  end;
  f:=BsfQWord(108544);
  if f<>11 then
  begin
    writeln('BsfQWord(108544) returned ',f,', should be 11');
    exit(false);
  end;
  f:=BsrQWord(2059);
  if f<>11 then
  begin
    writeln('BsrQWord(2059) returned ',f,', should be 11');
    exit(false);
  end;
  f:=BsfQWord(208896);
  if f<>12 then
  begin
    writeln('BsfQWord(208896) returned ',f,', should be 12');
    exit(false);
  end;
  f:=BsrQWord(4108);
  if f<>12 then
  begin
    writeln('BsrQWord(4108) returned ',f,', should be 12');
    exit(false);
  end;
  f:=BsfQWord(417792);
  if f<>13 then
  begin
    writeln('BsfQWord(417792) returned ',f,', should be 13');
    exit(false);
  end;
  f:=BsrQWord(8205);
  if f<>13 then
  begin
    writeln('BsrQWord(8205) returned ',f,', should be 13');
    exit(false);
  end;
  f:=BsfQWord(802816);
  if f<>14 then
  begin
    writeln('BsfQWord(802816) returned ',f,', should be 14');
    exit(false);
  end;
  f:=BsrQWord(16398);
  if f<>14 then
  begin
    writeln('BsrQWord(16398) returned ',f,', should be 14');
    exit(false);
  end;
  f:=BsfQWord(1605632);
  if f<>15 then
  begin
    writeln('BsfQWord(1605632) returned ',f,', should be 15');
    exit(false);
  end;
  f:=BsrQWord(32783);
  if f<>15 then
  begin
    writeln('BsrQWord(32783) returned ',f,', should be 15');
    exit(false);
  end;
  f:=BsfQWord(3080192);
  if f<>16 then
  begin
    writeln('BsfQWord(3080192) returned ',f,', should be 16');
    exit(false);
  end;
  f:=BsrQWord(65552);
  if f<>16 then
  begin
    writeln('BsrQWord(65552) returned ',f,', should be 16');
    exit(false);
  end;
  f:=BsfQWord(6160384);
  if f<>17 then
  begin
    writeln('BsfQWord(6160384) returned ',f,', should be 17');
    exit(false);
  end;
  f:=BsrQWord(131089);
  if f<>17 then
  begin
    writeln('BsrQWord(131089) returned ',f,', should be 17');
    exit(false);
  end;
  f:=BsfQWord(11796480);
  if f<>18 then
  begin
    writeln('BsfQWord(11796480) returned ',f,', should be 18');
    exit(false);
  end;
  f:=BsrQWord(262162);
  if f<>18 then
  begin
    writeln('BsrQWord(262162) returned ',f,', should be 18');
    exit(false);
  end;
  f:=BsfQWord(23592960);
  if f<>19 then
  begin
    writeln('BsfQWord(23592960) returned ',f,', should be 19');
    exit(false);
  end;
  f:=BsrQWord(524307);
  if f<>19 then
  begin
    writeln('BsrQWord(524307) returned ',f,', should be 19');
    exit(false);
  end;
  f:=BsfQWord(45088768);
  if f<>20 then
  begin
    writeln('BsfQWord(45088768) returned ',f,', should be 20');
    exit(false);
  end;
  f:=BsrQWord(1048596);
  if f<>20 then
  begin
    writeln('BsrQWord(1048596) returned ',f,', should be 20');
    exit(false);
  end;
  f:=BsfQWord(90177536);
  if f<>21 then
  begin
    writeln('BsfQWord(90177536) returned ',f,', should be 21');
    exit(false);
  end;
  f:=BsrQWord(2097173);
  if f<>21 then
  begin
    writeln('BsrQWord(2097173) returned ',f,', should be 21');
    exit(false);
  end;
  f:=BsfQWord(171966464);
  if f<>22 then
  begin
    writeln('BsfQWord(171966464) returned ',f,', should be 22');
    exit(false);
  end;
  f:=BsrQWord(4194326);
  if f<>22 then
  begin
    writeln('BsrQWord(4194326) returned ',f,', should be 22');
    exit(false);
  end;
  f:=BsfQWord(343932928);
  if f<>23 then
  begin
    writeln('BsfQWord(343932928) returned ',f,', should be 23');
    exit(false);
  end;
  f:=BsrQWord(8388631);
  if f<>23 then
  begin
    writeln('BsrQWord(8388631) returned ',f,', should be 23');
    exit(false);
  end;
  f:=BsfQWord(654311424);
  if f<>24 then
  begin
    writeln('BsfQWord(654311424) returned ',f,', should be 24');
    exit(false);
  end;
  f:=BsrQWord(16777240);
  if f<>24 then
  begin
    writeln('BsrQWord(16777240) returned ',f,', should be 24');
    exit(false);
  end;
  f:=BsfQWord(1308622848);
  if f<>25 then
  begin
    writeln('BsfQWord(1308622848) returned ',f,', should be 25');
    exit(false);
  end;
  f:=BsrQWord(33554457);
  if f<>25 then
  begin
    writeln('BsrQWord(33554457) returned ',f,', should be 25');
    exit(false);
  end;
  f:=BsfQWord(2483027968);
  if f<>26 then
  begin
    writeln('BsfQWord(2483027968) returned ',f,', should be 26');
    exit(false);
  end;
  f:=BsrQWord(67108890);
  if f<>26 then
  begin
    writeln('BsrQWord(67108890) returned ',f,', should be 26');
    exit(false);
  end;
  f:=BsfQWord(4966055936);
  if f<>27 then
  begin
    writeln('BsfQWord(4966055936) returned ',f,', should be 27');
    exit(false);
  end;
  f:=BsrQWord(134217755);
  if f<>27 then
  begin
    writeln('BsrQWord(134217755) returned ',f,', should be 27');
    exit(false);
  end;
  f:=BsfQWord(9395240960);
  if f<>28 then
  begin
    writeln('BsfQWord(9395240960) returned ',f,', should be 28');
    exit(false);
  end;
  f:=BsrQWord(268435484);
  if f<>28 then
  begin
    writeln('BsrQWord(268435484) returned ',f,', should be 28');
    exit(false);
  end;
  f:=BsfQWord(18790481920);
  if f<>29 then
  begin
    writeln('BsfQWord(18790481920) returned ',f,', should be 29');
    exit(false);
  end;
  f:=BsrQWord(536870941);
  if f<>29 then
  begin
    writeln('BsrQWord(536870941) returned ',f,', should be 29');
    exit(false);
  end;
  f:=BsfQWord(35433480192);
  if f<>30 then
  begin
    writeln('BsfQWord(35433480192) returned ',f,', should be 30');
    exit(false);
  end;
  f:=BsrQWord(1073741854);
  if f<>30 then
  begin
    writeln('BsrQWord(1073741854) returned ',f,', should be 30');
    exit(false);
  end;
  f:=BsfQWord(70866960384);
  if f<>31 then
  begin
    writeln('BsfQWord(70866960384) returned ',f,', should be 31');
    exit(false);
  end;
  f:=BsrQWord(2147483679);
  if f<>31 then
  begin
    writeln('BsrQWord(2147483679) returned ',f,', should be 31');
    exit(false);
  end;
  f:=BsfQWord(133143986176);
  if f<>32 then
  begin
    writeln('BsfQWord(133143986176) returned ',f,', should be 32');
    exit(false);
  end;
  f:=BsrQWord(4294967328);
  if f<>32 then
  begin
    writeln('BsrQWord(4294967328) returned ',f,', should be 32');
    exit(false);
  end;
  f:=BsfQWord(266287972352);
  if f<>33 then
  begin
    writeln('BsfQWord(266287972352) returned ',f,', should be 33');
    exit(false);
  end;
  f:=BsrQWord(8589934625);
  if f<>33 then
  begin
    writeln('BsrQWord(8589934625) returned ',f,', should be 33');
    exit(false);
  end;
  f:=BsfQWord(498216206336);
  if f<>34 then
  begin
    writeln('BsfQWord(498216206336) returned ',f,', should be 34');
    exit(false);
  end;
  f:=BsrQWord(17179869218);
  if f<>34 then
  begin
    writeln('BsrQWord(17179869218) returned ',f,', should be 34');
    exit(false);
  end;
  f:=BsfQWord(996432412672);
  if f<>35 then
  begin
    writeln('BsfQWord(996432412672) returned ',f,', should be 35');
    exit(false);
  end;
  f:=BsrQWord(34359738403);
  if f<>35 then
  begin
    writeln('BsrQWord(34359738403) returned ',f,', should be 35');
    exit(false);
  end;
  f:=BsfQWord(1855425871872);
  if f<>36 then
  begin
    writeln('BsfQWord(1855425871872) returned ',f,', should be 36');
    exit(false);
  end;
  f:=BsrQWord(68719476772);
  if f<>36 then
  begin
    writeln('BsrQWord(68719476772) returned ',f,', should be 36');
    exit(false);
  end;
  f:=BsfQWord(3710851743744);
  if f<>37 then
  begin
    writeln('BsfQWord(3710851743744) returned ',f,', should be 37');
    exit(false);
  end;
  f:=BsrQWord(137438953509);
  if f<>37 then
  begin
    writeln('BsrQWord(137438953509) returned ',f,', should be 37');
    exit(false);
  end;
  f:=BsfQWord(6871947673600);
  if f<>38 then
  begin
    writeln('BsfQWord(6871947673600) returned ',f,', should be 38');
    exit(false);
  end;
  f:=BsrQWord(274877906982);
  if f<>38 then
  begin
    writeln('BsrQWord(274877906982) returned ',f,', should be 38');
    exit(false);
  end;
  f:=BsfQWord(13743895347200);
  if f<>39 then
  begin
    writeln('BsfQWord(13743895347200) returned ',f,', should be 39');
    exit(false);
  end;
  f:=BsrQWord(549755813927);
  if f<>39 then
  begin
    writeln('BsrQWord(549755813927) returned ',f,', should be 39');
    exit(false);
  end;
  f:=BsfQWord(25288767438848);
  if f<>40 then
  begin
    writeln('BsfQWord(25288767438848) returned ',f,', should be 40');
    exit(false);
  end;
  f:=BsrQWord(1099511627816);
  if f<>40 then
  begin
    writeln('BsrQWord(1099511627816) returned ',f,', should be 40');
    exit(false);
  end;
  f:=BsfQWord(50577534877696);
  if f<>41 then
  begin
    writeln('BsfQWord(50577534877696) returned ',f,', should be 41');
    exit(false);
  end;
  f:=BsrQWord(2199023255593);
  if f<>41 then
  begin
    writeln('BsrQWord(2199023255593) returned ',f,', should be 41');
    exit(false);
  end;
  f:=BsfQWord(92358976733184);
  if f<>42 then
  begin
    writeln('BsfQWord(92358976733184) returned ',f,', should be 42');
    exit(false);
  end;
  f:=BsrQWord(4398046511146);
  if f<>42 then
  begin
    writeln('BsrQWord(4398046511146) returned ',f,', should be 42');
    exit(false);
  end;
  f:=BsfQWord(184717953466368);
  if f<>43 then
  begin
    writeln('BsfQWord(184717953466368) returned ',f,', should be 43');
    exit(false);
  end;
  f:=BsrQWord(8796093022251);
  if f<>43 then
  begin
    writeln('BsrQWord(8796093022251) returned ',f,', should be 43');
    exit(false);
  end;
  f:=BsfQWord(334251534843904);
  if f<>44 then
  begin
    writeln('BsfQWord(334251534843904) returned ',f,', should be 44');
    exit(false);
  end;
  f:=BsrQWord(17592186044460);
  if f<>44 then
  begin
    writeln('BsrQWord(17592186044460) returned ',f,', should be 44');
    exit(false);
  end;
  f:=BsfQWord(668503069687808);
  if f<>45 then
  begin
    writeln('BsfQWord(668503069687808) returned ',f,', should be 45');
    exit(false);
  end;
  f:=BsrQWord(35184372088877);
  if f<>45 then
  begin
    writeln('BsrQWord(35184372088877) returned ',f,', should be 45');
    exit(false);
  end;
  f:=BsfQWord(1196268651020288);
  if f<>46 then
  begin
    writeln('BsfQWord(1196268651020288) returned ',f,', should be 46');
    exit(false);
  end;
  f:=BsrQWord(70368744177710);
  if f<>46 then
  begin
    writeln('BsrQWord(70368744177710) returned ',f,', should be 46');
    exit(false);
  end;
  f:=BsfQWord(2392537302040576);
  if f<>47 then
  begin
    writeln('BsfQWord(2392537302040576) returned ',f,', should be 47');
    exit(false);
  end;
  f:=BsrQWord(140737488355375);
  if f<>47 then
  begin
    writeln('BsrQWord(140737488355375) returned ',f,', should be 47');
    exit(false);
  end;
  f:=BsfQWord(4222124650659840);
  if f<>48 then
  begin
    writeln('BsfQWord(4222124650659840) returned ',f,', should be 48');
    exit(false);
  end;
  f:=BsrQWord(281474976710704);
  if f<>48 then
  begin
    writeln('BsrQWord(281474976710704) returned ',f,', should be 48');
    exit(false);
  end;
  f:=BsfQWord(8444249301319680);
  if f<>49 then
  begin
    writeln('BsfQWord(8444249301319680) returned ',f,', should be 49');
    exit(false);
  end;
  f:=BsrQWord(562949953421361);
  if f<>49 then
  begin
    writeln('BsrQWord(562949953421361) returned ',f,', should be 49');
    exit(false);
  end;
  f:=BsfQWord(14636698788954112);
  if f<>50 then
  begin
    writeln('BsfQWord(14636698788954112) returned ',f,', should be 50');
    exit(false);
  end;
  f:=BsrQWord(1125899906842674);
  if f<>50 then
  begin
    writeln('BsrQWord(1125899906842674) returned ',f,', should be 50');
    exit(false);
  end;
  f:=BsfQWord(29273397577908224);
  if f<>51 then
  begin
    writeln('BsfQWord(29273397577908224) returned ',f,', should be 51');
    exit(false);
  end;
  f:=BsrQWord(2251799813685299);
  if f<>51 then
  begin
    writeln('BsrQWord(2251799813685299) returned ',f,', should be 51');
    exit(false);
  end;
  f:=BsfQWord(49539595901075456);
  if f<>52 then
  begin
    writeln('BsfQWord(49539595901075456) returned ',f,', should be 52');
    exit(false);
  end;
  f:=BsrQWord(4503599627370548);
  if f<>52 then
  begin
    writeln('BsrQWord(4503599627370548) returned ',f,', should be 52');
    exit(false);
  end;
  f:=BsfQWord(99079191802150912);
  if f<>53 then
  begin
    writeln('BsfQWord(99079191802150912) returned ',f,', should be 53');
    exit(false);
  end;
  f:=BsrQWord(9007199254741045);
  if f<>53 then
  begin
    writeln('BsrQWord(9007199254741045) returned ',f,', should be 53');
    exit(false);
  end;
  f:=BsfQWord(162129586585337856);
  if f<>54 then
  begin
    writeln('BsfQWord(162129586585337856) returned ',f,', should be 54');
    exit(false);
  end;
  f:=BsrQWord(18014398509482038);
  if f<>54 then
  begin
    writeln('BsrQWord(18014398509482038) returned ',f,', should be 54');
    exit(false);
  end;
  f:=BsfQWord(324259173170675712);
  if f<>55 then
  begin
    writeln('BsfQWord(324259173170675712) returned ',f,', should be 55');
    exit(false);
  end;
  f:=BsrQWord(36028797018964023);
  if f<>55 then
  begin
    writeln('BsrQWord(36028797018964023) returned ',f,', should be 55');
    exit(false);
  end;
  f:=BsfQWord(504403158265495552);
  if f<>56 then
  begin
    writeln('BsfQWord(504403158265495552) returned ',f,', should be 56');
    exit(false);
  end;
  f:=BsrQWord(72057594037927992);
  if f<>56 then
  begin
    writeln('BsrQWord(72057594037927992) returned ',f,', should be 56');
    exit(false);
  end;
  f:=BsfQWord(1008806316530991104);
  if f<>57 then
  begin
    writeln('BsfQWord(1008806316530991104) returned ',f,', should be 57');
    exit(false);
  end;
  f:=BsrQWord(144115188075855929);
  if f<>57 then
  begin
    writeln('BsrQWord(144115188075855929) returned ',f,', should be 57');
    exit(false);
  end;
  f:=BsfQWord(1441151880758558720);
  if f<>58 then
  begin
    writeln('BsfQWord(1441151880758558720) returned ',f,', should be 58');
    exit(false);
  end;
  f:=BsrQWord(288230376151711802);
  if f<>58 then
  begin
    writeln('BsrQWord(288230376151711802) returned ',f,', should be 58');
    exit(false);
  end;
  f:=BsfQWord(2882303761517117440);
  if f<>59 then
  begin
    writeln('BsfQWord(2882303761517117440) returned ',f,', should be 59');
    exit(false);
  end;
  f:=BsrQWord(576460752303423547);
  if f<>59 then
  begin
    writeln('BsrQWord(576460752303423547) returned ',f,', should be 59');
    exit(false);
  end;
  f:=BsfQWord(3458764513820540928);
  if f<>60 then
  begin
    writeln('BsfQWord(3458764513820540928) returned ',f,', should be 60');
    exit(false);
  end;
  f:=BsrQWord(1152921504606847036);
  if f<>60 then
  begin
    writeln('BsrQWord(1152921504606847036) returned ',f,', should be 60');
    exit(false);
  end;
  f:=BsfQWord(6917529027641081856);
  if f<>61 then
  begin
    writeln('BsfQWord(6917529027641081856) returned ',f,', should be 61');
    exit(false);
  end;
  f:=BsrQWord(2305843009213694013);
  if f<>61 then
  begin
    writeln('BsrQWord(2305843009213694013) returned ',f,', should be 61');
    exit(false);
  end;
  f:=BsfQWord(4611686018427387904);
  if f<>62 then
  begin
    writeln('BsfQWord(4611686018427387904) returned ',f,', should be 62');
    exit(false);
  end;
  f:=BsrQWord(4611686018427387966);
  if f<>62 then
  begin
    writeln('BsrQWord(4611686018427387966) returned ',f,', should be 62');
    exit(false);
  end;
  f:=BsfQWord(9223372036854775808);
  if f<>63 then
  begin
    writeln('BsfQWord(9223372036854775808) returned ',f,', should be 63');
    exit(false);
  end;
  f:=BsrQWord(9223372036854775871);
  if f<>63 then
  begin
    writeln('BsrQWord(9223372036854775871) returned ',f,', should be 63');
    exit(false);
  end;

  f:=BsfQWord(0);
  if (f<>$ff) then
  begin
    writeln('BsfQWord(0) returned ',f,', should be ',$ff);
    exit(false);
  end;
  r:=BsrQWord(0);
  if r<>$ff then
  begin
    writeln('BsrQWord(0) returned ',r,', should be ',$ff);
    exit(false);
  end;
  result:=true;
end;

begin
  if test_byte then writeln('passed') else halt(1);
  if test_word then writeln('passed') else halt(1);
  if test_dword then writeln('passed') else halt(1);
  if test_qword then writeln('passed') else halt(1);
end.
