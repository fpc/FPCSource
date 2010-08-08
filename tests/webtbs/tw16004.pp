{$apptype console}
{$mode Delphi}
{$assertions on}
{$codepage cp1251}

function verify(const p; const size: integer; const z: array of byte): boolean;
begin
	assert( size = length(z)*sizeof(z[0]) );
	result := CompareByte(p, z[0], size) = 0;
	writeln(result)
end;

	procedure foo;
	var a: array[0..5] of char = 'willow';
	const b: array[0..2] of WideChar = 'ива';
	begin
		assert( verify(a, sizeof(a), [ord('w'), ord('i'), ord('l'), ord('l'), ord('o'), ord('w')]) );
{$ifdef endian_big}
		assert( verify(b, sizeof(b), [$04,$38,$04,$32,$04,$30]) )
{$else}
		assert( verify(b, sizeof(b), [$38,$04,$32,$04,$30,$04]) )
{$endif}
	end;

const c: array[0..10] of char = 'rosenberg';
var d: array[0..10] of WideChar = 'розенберг';
	z: array[0..1] of WideChar = 'ы';
	x: array[0..0] of char = 'x';
begin
	assert( verify(c, sizeof(c), [114, 111, 115, 101, 110, 98, 101, 114, 103, 0, 0]) );
{$ifdef endian_big}
	assert( verify(d, sizeof(d), [$04,$40,$04,$3E,$04,$37,$04,$35,$04,$3D,$04,$31,$04,$35,$04,$40,$04,$33,0,0,0,0]) );
{$else}
	assert( verify(d, sizeof(d), [$40,$04,$3E,$04,$37,$04,$35,$04,$3D,$04,$31,$04,$35,$04,$40,$04,$33,$04,0,0,0,0]) );
{$endif}
	foo;
{$ifdef endian_big}
	assert( verify(z, sizeof(z), [$04,$4B,0,0]) );
{$else}
	assert( verify(z, sizeof(z), [$4B,$04,0,0]) );
{$endif}
	assert( verify(x, sizeof(x), [120]) )
end.