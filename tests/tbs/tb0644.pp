{ this test is geared towards Double values }

program tb0644;

{$mode objfpc}

uses
  Math, sysutils;

type
  TDataset = record
    Value: Double;
    AsIs: Double;
    More: Double;
    Less: Double;
    Exc: Boolean;
  end;

const
  DataSet: array[0..23] of TDataset = (
    (Value: 1.5;              AsIs: 1;   More: 2;     Less: 1;        Exc: False),
    (Value: 0;                AsIs: 0;     More: 0;   Less: 0;     Exc: False),
    (Value: 2251799813685248; AsIs: 2251799813685248;     More: 2251799813685248;   Less: 2251799813685247;      Exc: False),
    (Value: 4503599627370496; AsIs: 4503599627370496;     More: 4503599627370496;     Less: 4503599627370495;      Exc: False),
    (Value: 9223372036854775808.0;            AsIs: 9223372036854775808.0;     More: 9223372036854775808.0;     Less: 9223372036854775808.0;        Exc: False),
    (Value: 9223372036854775809.0;            AsIs: 9223372036854775809.0; More: 9223372036854775809.0; Less: 9223372036854775809.0;   Exc: False),
    (Value: 18446744073709551616.0; AsIs: 18446744073709551616.0; More: 18446744073709551616.0; Less: 18446744073709551616.0; Exc: False),
    (Value: -9223372036854775808.0;              AsIs: -9223372036854775808.0;   More: -9223372036854775808.0;     Less: -9223372036854775808.0;      Exc: False),
    (Value: -9223372036854775809.0; AsIs: -9223372036854775809.0;      More: -9223372036854775809.0;  Less: -9223372036854775809.0;   Exc: False),
    (Value: -18446744073709551616.0; AsIs: -18446744073709551616.0;      More: -18446744073709551616.0;  Less: -18446744073709551616.0;      Exc: False),
    (Value: 1E300;            AsIs: 1E300;      More: 1E300;     Less: 1E300;      Exc: False),
    (Value: 0.125; AsIs: 0; More: 0; Less: 0; Exc: False),
    (Value: 3.6415926535897932384626433832795;            AsIs: 3; More: 4; Less: 3; Exc: False),
    (Value: -1.5; AsIs: -1; More: -1; Less: -2; Exc: False),
    (Value: -2251799813685248; AsIs: -2251799813685248; More: -2251799813685247; Less: -2251799813685248; Exc: False),
    (Value: -4503599627370496; AsIs: -4503599627370496; More: -4503599627370495; Less: -4503599627370496; Exc: False),
    (Value: -1E300; AsIs: -1E300; More: -1E300; Less: -1E300; Exc: False),
    (Value: -0.125; AsIs: 0; More: 0; Less: 0; Exc: False),
    (Value: -3.6415926535897932384626433832795; AsIs: -3; More: -3; Less: -4; Exc: False),
    (Value: 1E1000; AsIs: 1E1000; More: 1E1000; Less: 1E1000; Exc: False),
    (Value: -1E1000; AsIs: -1E1000; More: -1E1000; Less: -1E1000; Exc: False),
    (Value: Infinity;          AsIs: Infinity;    More: Infinity;   Less: Infinity;    Exc: False),
    (Value: NegInfinity;       AsIs: NegInfinity;    More: NegInfinity;   Less: NegInfinity;    Exc: False),
    (Value: NaN;               AsIs: NaN;    More: NaN;   Less: NaN;    Exc: False)
  );

function SameValue(aGot, aExpected: Double): Boolean;
begin
  if IsNan(aExpected) then
    Result := IsNan(aGot)
  else
    Result := aGot = aExpected;
end;

var
  ds: TDataSet;
  v: Double;
  hadexc: Boolean;
  orgmask: TFPUExceptionMask;
begin
{$if defined(FPC_HAS_TYPE_EXTENDED) or not defined(FPC_HAS_TYPE_DOUBLE)}
  { we rely on the floating point values to be doubles, so only test on systems
    that use double as their largest type }
  Exit;
{$endif}

  orgmask := GetExceptionMask;

  Writeln('Testing with exceptions disabled');
  SetExceptionMask(orgmask + [exPrecision, exInvalidOp]);
  for ds in DataSet do begin
    Writeln('Testing value ', ds.Value);
    v := Int(ds.Value);
    if not SameValue(v, ds.AsIs) then begin
      Writeln('Int(', ds.Value, ') failed: expected ', ds.AsIs, ', but got ', v);
      Halt(1);
    end;
    v := Int(ds.Value + 0.5);
    if not SameValue(v, ds.More) then begin
      Writeln('Int(', ds.Value, ' + 0.5) failed: expected ', ds.More, ', but got ', v);
      Halt(2);
    end;
    v := Int(ds.Value - 0.5);
    if not SameValue(v, ds.Less) then begin
      Writeln('Int(', ds.Value, ' - 0.5) failed: expected ', ds.Less, ', but got ', v);
      Halt(3);
    end;
  end;

  Writeln('Testing with exceptions enabled');
  SetExceptionMask(orgmask);

  for ds in DataSet do begin
    hadexc := False;
    try
      Writeln('Testing value ', ds.Value);
      v := Int(ds.Value);
      if not SameValue(v, ds.AsIs) then begin
        Writeln('Int(', ds.Value, ') failed: expected ', ds.AsIs, ', but got ', v);
        Halt(1);
      end;
      v := Int(ds.Value + 0.5);
      if not SameValue(v, ds.More) then begin
        Writeln('Int(', ds.Value, ' + 0.5) failed: expected ', ds.More, ', but got ', v);
        Halt(2);
      end;
      v := Int(ds.Value - 0.5);
      if not SameValue(v, ds.Less) then begin
        Writeln('Int(', ds.Value, ' - 0.5) failed: expected ', ds.Less, ', but got ', v);
        Halt(3);
      end;
    except
      on e: EMathError do begin
        if ds.Exc then begin
          Writeln('Got expected exception for value ', ds.Value);
          hadexc := True;
        end else
          Writeln('Unexpected math exception for value ', ds.Value, ': ', e.Message);
      end else
        Writeln('Unexpected exception for value ', ds.Value, ': ', ExceptObject.ClassName);
    end;
    if ds.Exc and not hadexc then begin
      Writeln('Exception expected, but none caught');
      Halt(4);
    end;
  end;

  Writeln('ok');
end.
