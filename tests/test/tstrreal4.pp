program tstrreal4;
{ test for issue #13722 by Zeljan Rikalo}
uses SysUtils;

procedure test;
var
 s: string;
 r: double;
 DS: Char;
begin
 DecimalSeparator := '.';
 DS := DecimalSeparator;
 r := 0.001;
 s := FloatToStrF(r, ffGeneral, 12, 2);
 {must print 0.001  }
 writeln(s);
 if (s <> '0'+DS+'001') then
   halt(1);

 s := FloatToStrF(r, ffFixed, 12, 2);
 {must print 0.00  }
 writeln(s);
 if (s <> '0'+DS+'00') then
   halt(1);

 s := FloatToStrF(r, ffNumber, 12, 2);
 {must print 0.00  }
 writeln(s);
 if (s <> '0'+DS+'00') then
   halt(1);

 r := -0.00001;

 s := FloatToStrF(r, ffGeneral, 12, 2);
 {must print -0.00001  }
 writeln(s);
 {$IFDEF FPC}
 if (s <> '-0'+DS+'00001') then
 {$ELSE}
 if (s <> '-1E-05') then // is this DCC bug ?
 {$ENDIF}
   halt(1);

 s := FloatToStrF(r, ffExponent, 12, 2);
 {must print -1.00000000000E-05  }
 writeln(s);
 if (s <> '-1'+DS+'00000000000E-05') then
   halt(1);

 s := FloatToStrF(r, ffFixed, 12, 2);
 {must print 0.00  }
 writeln(s);
 if (s <> '0'+DS+'00') then
   halt(1);

 s := FloatToStrF(r, ffNumber, 12, 2);
 {must print 0.00  }
 writeln(s);
 if (s <> '0'+DS+'00') then
   halt(1);

 s := FloatToStrF(r, ffCurrency, 12, 2);
 {must print without leading zero  }
 writeln(s);
 if (length(s) > 0) and 
  ((Pos('-', s) > 0) or ((Pos('(', s) > 0) and  (Pos(')', s) > 0))) then
   halt(1);

 r := -0.00000;

 s := FloatToStrF(r, ffGeneral, 12, 2);
 {must print 0  }
 writeln(s);
 if (s <> '0') then
   halt(1);

 s := FloatToStrF(r, ffExponent, 12, 2);
 {must print 0.00  }
 writeln(s);
 if (s <> '0'+DS+'00000000000E+00') then
   halt(1);

 s := FloatToStrF(r, ffFixed, 12, 2);
 {must print 0.00  }
 writeln(s);
 if (s <> '0'+DS+'00') then
   halt(1);

 s := FloatToStrF(r, ffNumber, 12, 2);
 {must print 0.00  }
 writeln(s);
 if (s <> '0'+DS+'00') then
   halt(1);

 s := FloatToStrF(r, ffCurrency, 12, 2);
 {must print without leading zero  }
 writeln(s);
 if (length(s) > 0) and 
  ((Pos('-', s) > 0) or ((Pos('(', s) > 0) and  (Pos(')', s) > 0))) then
   halt(1);

 // Now check if we remove leading negative sign by mistake
 r := -0.00001;

 s := FloatToStrF(r, ffGeneral, 12, 5);
 {must print -0.00001  }
 writeln(s);
 {$IFDEF FPC}
 if (s <> '-0'+DS+'00001') then
 {$ELSE}
 if (s <> '-1E-5') then // is this DCC bug ?
 {$ENDIF}
   halt(1);

 s := FloatToStrF(r, ffExponent, 12, 5);
 {must print -0.00001  }
 writeln(s);
 {$IFDEF FPC}
 if (s <> '-1'+DS+'00000000000E-0005') then
 {$ELSE}
 if (s <> '-1.00000000000E-5') then
 {$ENDIF}
   halt(1);

 s := FloatToStrF(r, ffFixed, 12, 5);
 {must print 0.00  }
 writeln(s);
 if (s <> '-0'+DS+'00001') then
   halt(1);

 s := FloatToStrF(r, ffNumber, 12, 5);
 {must print 0.00  }
 writeln(s);
 if (s <> '-0'+DS+'00001') then
   halt(1);

 s := FloatToStrF(r, ffCurrency, 12, 5);

 {here we check for various currency negative formats.
 There's bug in rtl cause NegCurFormat can be > 10
 and in that case it isn't handled by FloatToStrIntl().
 So that's why we check NegCurFormat range here.}

 writeln(s);

 if (length(s) > 0) and (NegCurrFormat in [0..10]) and
   (Pos('-', s) = 0) and (Pos('(', s) = 0) and  (Pos(')', s) = 0) then
   halt(1);
 writeln('Tests for FloatToStrF(): SUCCESS');
end;

begin
  test;
end.
