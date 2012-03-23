{ Source provided for Free Pascal Bug Report 3973 }
{ Submitted by "alphax" on  2005-05-16 }
{ e-mail: graphcoloring@yahoo.com.cn }
program fpc_test_3;

{$R-}
{$Q-}

{$APPTYPE CONSOLE}

{$IFDEF FPC}
  {$MODE objfpc}
{$ENDIF}

uses
  SysUtils, Variants;

var
  FailureCount: Integer;

  procedure TestOpenArray;

    procedure p(const a: array of const);

      procedure Check(
              const TypeName: string;
              const aVarRec: TVarRec;
              const aExpectedVType: Byte
            );
      begin
        Write('VType of ', TypeName, ' element is: ', aVarRec.VType, '--------');
        if aVarRec.VType = aExpectedVType then
          WriteLn('Ok')
        else
        begin
          Inc(FailureCount);
          WriteLn('Failure');
        end;
      end;

    begin
      Check('Currency', a[0], vtCurrency);
      Check('Interface(nil)', a[1], vtInterface);
      Check('Interface', a[2], vtInterface);
      { TObject is a class as well! }
      Check('Class Object(nil)', a[3], vtObject);
      Check('Class', a[4], vtClass);
      {$IFDEF FPC}
      Check('QWord', a[5], vtQWord);
      {$ENDIF}

      { I WISH FPC Introduce a vtDateTime for the TDatetime parameter }
    end;

  var
    C: Currency;
    DT: TDateTime;
    IntfNil, Intf: IInterface;
    Obj: TObject;

    {$IFDEF FPC}
    Quad: QWord;
    {$ENDIF}
  begin
    C := 0;
    IntfNil := nil;
    Intf := TInterfacedObject.Create();
    Obj := nil;
    {$IFDEF FPC}
    Quad := 0;
    p([C, IntfNil, Intf, Obj, TObject, Quad]);
    {$ELSE}
    p([C, IntfNil, Intf, Obj, TObject]);
    {$ENDIF}
  end;


  procedure TestVarType;

    procedure Check(
            const aTypeName: string;
            const V: Variant;
            const aExpectedVarType: TVarType);
    var
      VT: TVarType;
    begin
      VT := VarType(V);
      Write('VarType of ', aTypeName, ' variant is: ', VT, '--------');
      if VT = aExpectedVarType then
        Writeln('Ok')
      else
      begin
        WriteLn('Failure');
        Inc(FailureCount);
      end;
    end;

  var
    C: Currency;
    DT: TDateTime;
    Intf: IInterface;
    {$IFDEF FPC}
    Quad: QWord;
    {$ENDIF}
  begin
    C := 0;
    DT := 0;
    Intf := TInterfacedObject.Create();
    {$IFDEF FPC}
    Quad := 0;
    {$ENDIF}
    Check('Currency', C, varCurrency);
    Check('Datetime', DT, varDate);
    Check('Interface', Intf, varUnknown);
    {$IFDEF FPC}
    Check('QWord', Quad, varQWord);
    {$ENDIF}
  end;

  procedure TestFormat;
  var
    uLong: Longword;
    Longlong: Int64;
    {$IFDEF FPC}
    Quad: QWord;
    {$ENDIF}
  begin
    uLong := High(uLong);
    Writeln(Format('high of longword is: %u', [uLong]), ' ', IntToHex(ulong, 8));
    Longlong := High(Longlong);
    Writeln(Format('high of int64 is: %d', [Longlong]), ' ', IntToHex(Longlong, 16));
    {$IFDEF FPC}
    Quad := High(Quad);
    Writeln(Format('high of quadword is: %u', [Quad]), ' ', IntToHex(int64(Quad), 16));
    {$ENDIF}
  end;

begin
  TestOpenArray();
  TestVarType();
  TestFormat();
  WriteLn;
  if FailureCount = 0 then
    WriteLn('All passed')
  else
    begin
      WriteLn(FailureCount, 'Failures');
      halt(1);
    end;
end.
