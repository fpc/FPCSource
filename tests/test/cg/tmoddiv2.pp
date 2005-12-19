program mymodtest;

{$MODE DELPHI}
{$ASSERTIONS ON}

// Pascal implementation of signed modulus by power of 2 constant algorithm
function my_modulus(x, m : integer) : integer;
var
	temp, mask1, mask2 : integer;
begin
	m := abs(m-1);
	
	temp := x and m;

	if (x < 0) then begin // = sign bit
		mask2 := -1;
	end else begin
		mask2 := 0;
	end;

	if (temp <> 0) then begin // note: temp >= 0
		mask1 := -1;
	end else begin
		mask1 := 0;
	end;
	
	my_modulus := temp or ((not m) and mask1 and mask2);	
end;

function i32_modulus(x, m : integer) : integer;
var
    temp : integer;
begin
    temp := x div m;
    i32_modulus := x - (temp*m);
end;

function u32_modulus(x, m : dword) : dword;
var
	temp : dword;
begin
	temp := x div m;
	u32_modulus := x - (temp*m);
end;

var
	i : integer;
	j, k : longint;
	res, res2 : longint;
	
	y, z : dword;

begin
	randseed := 1; // just take any, but repeatable
	write('positive int32 division test...');
	for i := -10000 to 10000 do begin
		j := random(high(integer));
		if (random(2) = 1) then j := -j;
		k := 19;
		assert((j div 19) = (j div k), 'Wrong int32 division by 19 for j=' + hexstr(j,sizeof(j)*2) + ' k=' + hexstr(k, sizeof(k)*2));
	end;
	writeln('Success.');
	
	write('Negative int32 division test...');
	for i := -10000 to 10000 do begin
		j := random(high(integer));
		if (random(2) = 1) then j := -j;
		k := -19;
		assert((j div -19) = (j div k), 'Wrong int32 division by -19 for j=' + hexstr(j,sizeof(j)*2) + ' k=' + hexstr(k, sizeof(k)*2));
	end;
	writeln('Success.');

	write('positive int32 division test...');
	for i := -10000 to 10000 do begin
		j := random(high(integer));
		if (random(2) = 1) then j := -j;
		k := 3;
		assert((j div 3) = (j div k), 'Wrong int32 division by 3 for j=' + hexstr(j,sizeof(j)*2) + ' k=' + hexstr(k, sizeof(k)*2));
	end;
	writeln('Success.');
	
	write('Negative int32 division test...');
	for i := -10000 to 10000 do begin
		j := random(high(integer));
		if (random(2) = 1) then j := -j;
		k := -3;
		assert((j div -3) = (j div k), 'Wrong int32 division by -3 for j=' + hexstr(j,sizeof(j)*2) + ' k=' + hexstr(k, sizeof(k)*2));
	end;
	writeln('Success.');

	write('positive int32 division test...');
	for i := -10000 to 10000 do begin
		j := random(high(integer));
		if (random(2) = 1) then j := -j;
		k := 7;
		assert((j div 7) = (j div k), 'Wrong int32 division by 7 for j=' + hexstr(j,sizeof(j)*2) + ' k=' + hexstr(k, sizeof(k)*2));
	end;
	writeln('Success.');
	
	write('Negative int32 division test...');
	for i := -10000 to 10000 do begin
		j := random(high(integer));
		if (random(2) = 1) then j := -j;
		k := -7;
		assert((j div -7) = (j div k), 'Wrong int32 division by -7 for j=' + hexstr(j,sizeof(j)*2) + ' k=' + hexstr(k, sizeof(k)*2));
	end;
	writeln('Success.');
	
	write('positive int32 division test...');
	for i := -10000 to 10000 do begin
		j := random(high(integer));
		if (random(2) = 1) then j := -j;
		k := 5;
		assert((j div 5) = (j div k), 'Wrong int32 division by 5 for j=' + hexstr(j,sizeof(j)*2) + ' k=' + hexstr(k, sizeof(k)*2));
	end;
	writeln('Success.');
	
	write('Negative int32 division test...');
	for i := -10000 to 10000 do begin
		j := random(high(integer));
		if (random(2) = 1) then j := -j;
		k := -5;
		assert((j div -5) = (j div k), 'Wrong int32 division by -5 for j=' + hexstr(j,sizeof(j)*2) + ' k=' + hexstr(k, sizeof(k)*2));
	end;
	writeln('Success.');
	
	write('positive int32 division test...');
	for i := -10000 to 10000 do begin
		j := random(high(integer));
		if (random(2) = 1) then j := -j;
		k := 512;
		assert((j div 512) = (j div k), 'Wrong int32 division by 512 for j=' + hexstr(j,sizeof(j)*2) + ' k=' + hexstr(k, sizeof(k)*2));
	end;
	writeln('Success.');
	
	write('Negative int32 division test...');
	for i := -10000 to 10000 do begin
		j := random(high(integer));
		if (random(2) = 1) then j := -j;
		k := -512;
		assert((j div -512) = (j div k), 'Wrong int32 division by -512 for j=' + hexstr(j,sizeof(j)*2) + ' k=' + hexstr(k, sizeof(k)*2));
	end;
	writeln('Success.');
	
//-----------------------------------------------------------------
	
	write('positive int32 modulus test (19)...');
	for i := -10000 to 10000 do begin
		j := random(high(integer));
		if (random(2) = 1) then j := -j;
		k := 19;
		assert((j mod 19) = (i32_modulus(j,k)), 'Wrong int32 modulus by 19 for j=' + hexstr(j,sizeof(j)*2) + ' k=' + hexstr(k, sizeof(k)*2));
	end;
	writeln('Success.');
	

	write('Negative int32 modulus test (-19)...');
	for i := -10000 to 10000 do begin
		j := random(high(integer));
		if (random(2) = 1) then j := -j;
		k := -19;
		res := j mod -19;
		res2 := i32_modulus(j, k);
		assert((res = res2), 'Int32 mod by -19 j=' + hexstr(j,sizeof(j)*2) + ' k=' + hexstr(k, sizeof(k)*2) + ' is ' + hexstr(res, 8) + ' ' + hexstr(res2, 8));
	end;
	writeln('Success.');
	
	write('positive int32 modulus test (3)...');
	for i := -10000 to 10000 do begin
		j := random(high(integer));
		if (random(2) = 1) then j := -j;
		k := 3;
		assert((j mod 3) = (i32_modulus(j,k)), 'Wrong int32 modulus by 3 for j=' + hexstr(j,sizeof(j)*2) + ' k=' + hexstr(k, sizeof(k)*2));
	end;
	writeln('Success.');
	

	write('Negative int32 modulus test (-3)...');
	for i := -10000 to 10000 do begin
		j := random(high(integer));
		if (random(2) = 1) then j := -j;
		k := -3;
		res := j mod -3;
		res2 := i32_modulus(j, k);
		assert((res = res2), 'Int32 mod by -3 j=' + hexstr(j,sizeof(j)*2) + ' k=' + hexstr(k, sizeof(k)*2) + ' is ' + hexstr(res, 8) + ' ' + hexstr(res2, 8));
	end;
	writeln('Success.');
	
	write('positive int32 modulus test (5)...');
	for i := -10000 to 10000 do begin
		j := random(high(integer));
		if (random(2) = 1) then j := -j;
		k := 5;
		assert((j mod 5) = (i32_modulus(j,k)), 'Wrong int32 modulus by 5 for j=' + hexstr(j,sizeof(j)*2) + ' k=' + hexstr(k, sizeof(k)*2));
	end;
	writeln('Success.');
	

	write('Negative int32 modulus test (-5)...');
	for i := -10000 to 10000 do begin
		j := random(high(integer));
		if (random(2) = 1) then j := -j;
		k := -5;
		res := j mod -5;
		res2 := i32_modulus(j, k);
		assert((res = res2), 'Int32 mod by -5 j=' + hexstr(j,sizeof(j)*2) + ' k=' + hexstr(k, sizeof(k)*2) + ' is ' + hexstr(res, 8) + ' ' + hexstr(res2, 8));
	end;
	writeln('Success.');
	
	write('positive int32 modulus test (7)...');
	for i := -10000 to 10000 do begin
		j := random(high(integer));
		if (random(2) = 1) then j := -j;
		k := 7;
		assert((j mod 7) = (i32_modulus(j,k)), 'Wrong int32 modulus by 7 for j=' + hexstr(j,sizeof(j)*2) + ' k=' + hexstr(k, sizeof(k)*2));
	end;
	writeln('Success.');
	

	write('Negative int32 modulus test (-7)...');
	for i := -10000 to 10000 do begin
		j := random(high(integer));
		if (random(2) = 1) then j := -j;
		k := -7;
		res := j mod -7;
		res2 := i32_modulus(j, k);
		assert((res = res2), 'Int32 mod by -7 j=' + hexstr(j,sizeof(j)*2) + ' k=' + hexstr(k, sizeof(k)*2) + ' is ' + hexstr(res, 8) + ' ' + hexstr(res2, 8));
	end;
	writeln('Success.');
	
	write('positive int32 modulus test (512)...');
	for i := -10000 to 10000 do begin
		j := random(high(integer));
		if (random(2) = 1) then j := -j;
		k := 512;
		assert((j mod 512) = (i32_modulus(j,k)), 'Wrong int32 modulus by 512 for j=' + hexstr(j,sizeof(j)*2) + ' k=' + hexstr(k, sizeof(k)*2));
	end;
	writeln('Success.');
	

	write('Negative int32 modulus test (-512)...');
	for i := -10000 to 10000 do begin
		j := random(high(integer));
		if (random(2) = 1) then j := -j;
		k := -512;
		res := j mod -512;
		res2 := i32_modulus(j, k);
		assert((res = res2), 'Int32 mod by -512 j=' + hexstr(j,sizeof(j)*2) + ' k=' + hexstr(k, sizeof(k)*2) + ' is ' + hexstr(res, 8) + ' ' + hexstr(res2, 8));
	end;
	writeln('Success.');
	
	write('positive uint32 division test (19)...');
	for i := -10000 to 10000 do begin
		y := random(high(integer));
		if (random(2) = 1) then y := 2 * y;
		z := 19;
		assert((y div 19) = (y div z), 'Wrong uint32 division by 19 for y=' + hexstr(y,sizeof(y)*2) + ' z=' + hexstr(z, sizeof(z)*2));
	end;
	writeln('Success.');
	
	write('positive uint32 modulus test (19)...');
	for i := -10000 to 10000 do begin
		y := random(high(integer));
		if (random(2) = 1) then y := 2 * y;
		z := 19;
		assert((y mod 19) = (u32_modulus(y,z)), 'Wrong uint32 modulus by 19 for y=' + hexstr(y,sizeof(y)*2) + ' z=' + hexstr(z, sizeof(z)*2));
	end;
	writeln('Success.');
	
	write('positive uint32 division test (3)...');
	for i := -10000 to 10000 do begin
		y := random(high(integer));
		if (random(2) = 1) then y := 2 * y;
		z := 3;
		assert((y div 3) = (y div z), 'Wrong uint32 division by 3 for y=' + hexstr(y,sizeof(y)*2) + ' z=' + hexstr(z, sizeof(z)*2));
	end;
	writeln('Success.');

	write('positive uint32 modulus test (3)...');
	for i := -10000 to 10000 do begin
		y := random(high(integer));
		if (random(2) = 1) then y := 2 * y;
		z := 3;
		assert((y mod 3) = (u32_modulus(y,z)), 'Wrong uint32 modulus by 3 for y=' + hexstr(y,sizeof(y)*2) + ' z=' + hexstr(z, sizeof(z)*2));
	end;
	writeln('Success.');
	
	write('positive uint32 division test (5)...');
	for i := -10000 to 10000 do begin
		y := random(high(integer));
		if (random(2) = 1) then y := 2 * y;
		z := 5;
		assert((y div 5) = (y div z), 'Wrong uint32 division by 5 for y=' + hexstr(y,sizeof(y)*2) + ' z=' + hexstr(z, sizeof(z)*2));
	end;
	writeln('Success.');
	
	write('positive uint32 modulus test (5)...');
	for i := -10000 to 10000 do begin
		y := random(high(integer));
		if (random(2) = 1) then y := 2 * y;
		z := 5;
		assert((y mod 5) = (u32_modulus(y,z)), 'Wrong uint32 modulus by 5 for y=' + hexstr(y,sizeof(y)*2) + ' z=' + hexstr(z, sizeof(z)*2));
	end;
	writeln('Success.');
	
	write('positive uint32 division test...');
	for i := -10000 to 10000 do begin
		y := random(high(integer));
		if (random(2) = 1) then y := 2 * y;
		z := 7;
		assert((y div 7) = (y div z), 'Wrong uint32 division by 7 for y=' + hexstr(y,sizeof(y)*2) + ' z=' + hexstr(z, sizeof(z)*2));
	end;
	writeln('Success.');
	
	write('positive uint32 modulus test...');
	for i := -10000 to 10000 do begin
		y := random(high(integer));
		if (random(2) = 1) then y := 2 * y;
		z := 7;
		assert((y mod 7) = (u32_modulus(y,z)), 'Wrong uint32 modulus by 7 for y=' + hexstr(y,sizeof(y)*2) + ' z=' + hexstr(z, sizeof(z)*2));
	end;
	writeln('Success.');	
	

	write('positive uint32 division test...');
	for i := -10000 to 10000 do begin
		y := random(high(integer));
		if (random(2) = 1) then y := 2 * y;
		z := 512;
		assert((y div 512) = (y div z), 'Wrong uint32 division by 512 for y=' + hexstr(y,sizeof(y)*2) + ' z=' + hexstr(z, sizeof(z)*2));
	end;
	writeln('Success.');

	write('positive uint32 modulus test...');
	for i := -10000 to 10000 do begin
		y := random(high(integer));
		if (random(2) = 1) then y := 2 * y;
		z := 512;
		assert((y mod 512) = (u32_modulus(y,z)), 'Wrong uint32 modulus by 512 for y=' + hexstr(y,sizeof(y)*2) + ' z=' + hexstr(z, sizeof(z)*2));
	end;
	writeln('Success.');

        { extra test for div by constant optimization }
	write('positive uint32 division test...');
	for i := -10000 to 10000 do begin
		y := random(high(integer));
		if (random(2) = 1) then y := 2 * y;
		z := $deadbeef;
		assert((y div $deadbeef) = (y div z), 'Wrong uint32 division by $deadbeaf for y=' + hexstr(y,sizeof(y)*2) + ' z=' + hexstr(z, sizeof(z)*2));
	end;
	writeln('Success.');

	write('positive uint32 division test...');
	for i := -10000 to 10000 do begin
		y := random(high(integer));
		if (random(2) = 1) then y := 2 * y;
		z := $b16beef;
		assert((y div $b16beef) = (y div z), 'Wrong uint32 division by $b16beef for y=' + hexstr(y,sizeof(y)*2) + ' z=' + hexstr(z, sizeof(z)*2));
	end;
	writeln('Success.');
end.

