uses
  SysUtils;

const
  results: array[1..16] of string = 
(
'234.6',
'234.57',
'234.568',
'1000',
'1235',
'1234.6',
'1234.57',
'1234.568',
'-234.6',
'-234.57',
'-234.568',
'-1000',
'-1235',
'-1234.6',
'-1234.57',
'-1234.568'
);

procedure check(const s: string; index: longint);
begin
  if (s<>results[index]) then
    begin
      writeln('Expected : ',results[index]);
      writeln('Got      : ',s);
      halt(1);
    end;
end;

var ext:extended;
  str: shortstring;
begin
  DecimalSeparator:='.';
  ext:=234.56789; 
  check(FloatToStrF(ext,ffGeneral,4,1),1);
  check(FloatToStrF(ext,ffGeneral,5,1),2);
  check(FloatToStrF(ext,ffGeneral,6,7),3);
  ext:=999.9999;
  check(FloatToStrF(ext,ffGeneral,4,7),4);
  ext:=1234.56789;
  check(FloatToStrF(ext,ffGeneral,4,1),5);
  check(FloatToStrF(ext,ffGeneral,5,1),6);
  check(FloatToStrF(ext,ffGeneral,6,1),7);
  check(FloatToStrF(ext,ffGeneral,7,1),8);

  ext:=-234.56789; 
  check(FloatToStrF(ext,ffGeneral,4,1),9);
  check(FloatToStrF(ext,ffGeneral,5,1),10);
  check(FloatToStrF(ext,ffGeneral,6,7),11);
  ext:=-999.9999;
  check(FloatToStrF(ext,ffGeneral,4,7),12);
  ext:=-1234.56789;
  check(FloatToStrF(ext,ffGeneral,4,1),13);
  check(FloatToStrF(ext,ffGeneral,5,1),14);
  check(FloatToStrF(ext,ffGeneral,6,1),15);
  check(FloatToStrF(ext,ffGeneral,7,1),16);
end.

