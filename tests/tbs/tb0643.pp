{ %OPT=-CE }
{ this test is geared towards Double values }

program tb0643;

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

var
  DataSet: array[0..15] of TDataset = (
    (Value: 1.5;              AsIs: 0.5;   More: 0;     Less: 0;        Exc: False),
    (Value: 0;                AsIs: 0;     More: 0.5;   Less: -0.5;     Exc: False),
    (Value: 2251799813685248; AsIs: 0;     More: 0.5;   Less: 0.5;      Exc: False),
    (Value: 4503599627370496; AsIs: 0;     More: 0;     Less: 0.5;      Exc: False),
    (Value: 1E300;            AsIs: 0;     More: 0;     Less: 0;        Exc: False),
    (Value: 0.125;            AsIs: 0.125; More: 0.625; Less: -0.375;   Exc: False),
    (Value: 3.6415926535897932384626433832795; AsIs: 0.64159265358979312; More: 0.14159265358979312; Less: 0.14159265358979312; Exc: False),
    (Value: -1.5;              AsIs: -0.5;   More: 0;     Less: 0;      Exc: False),
    (Value: -2251799813685248; AsIs: 0;      More: -0.5;  Less: -0.5;   Exc: False),
    (Value: -4503599627370496; AsIs: 0;      More: -0.5;  Less: 0;      Exc: False),
    (Value: -1E300;            AsIs: 0;      More: 0;     Less: 0;      Exc: False),
    (Value: -0.125;            AsIs: -0.125; More: 0.375; Less: -0.625; Exc: False),
    (Value: -3.6415926535897932384626433832795; AsIs: -0.64159265358979312; More: -0.14159265358979312; Less: -0.14159265358979312; Exc: False),
    (Value: Infinity;          AsIs: NaN;    More: NaN;   Less: NaN;    Exc: True),
    (Value: NegInfinity;       AsIs: NaN;    More: NaN;   Less: NaN;    Exc: True),
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
    v := Frac(ds.Value);
    if not SameValue(v, ds.AsIs) then begin
      Writeln('Frac(', ds.Value, ') failed: expected ', ds.AsIs, ', but got ', v);
      Halt(1);
    end;
    v := Frac(ds.Value + 0.5);
    if not SameValue(v, ds.More) then begin
      Writeln('Frac(', ds.Value, ' + 0.5) failed: expected ', ds.More, ', but got ', v);
      Halt(2);
    end;
    v := Frac(ds.Value - 0.5);
    if not SameValue(v, ds.Less) then begin
      Writeln('Frac(', ds.Value, ' - 0.5) failed: expected ', ds.Less, ', but got ', v);
      Halt(3);
    end;
  end;

  Writeln('Testing with exceptions enabled');
  SetExceptionMask(orgmask);

  for ds in DataSet do begin
    hadexc := False;
    try
      Writeln('Testing value ', ds.Value);
      v := Frac(ds.Value);
      if not SameValue(v, ds.AsIs) then begin
        Writeln('Frac(', ds.Value, ') failed: expected ', ds.AsIs, ', but got ', v);
        Halt(1);
      end;
      v := Frac(ds.Value + 0.5);
      if not SameValue(v, ds.More) then begin
        Writeln('Frac(', ds.Value, ' + 0.5) failed: expected ', ds.More, ', but got ', v);
        Halt(2);
      end;
      v := Frac(ds.Value - 0.5);
      if not SameValue(v, ds.Less) then begin
        Writeln('Frac(', ds.Value, ' - 0.5) failed: expected ', ds.Less, ', but got ', v);
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
