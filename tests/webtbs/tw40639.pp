{ %NORUN }

program tw40639;
{$mode objfpc}
{$ModeSwitch typehelpers}
type
  TDBGPtr = qword;

  TFpBreakPointMap = class abstract
  strict protected type
    { TFpBreakPointMapEntry }
    TFpBreakPointMapEntry = packed record
      InternalBreakPoint: Pointer;  // TFpInternalBreakpoint or TFpInternalBreakpointArray
      IsBreakList: ByteBool;
    end;
    PFpBreakPointMapEntry = ^TFpBreakPointMapEntry;

    { TFpBreakPointMapLocDataPair }
    TFpBreakPointMapLocDataPair = record
      Location: TDBGPtr;
      Data: PFpBreakPointMapEntry;
    end;

    //TFpBreakPointMapLocDataPairHelper = type helper for TFpBreakPointMapLocDataPair
    //  function OrigValue: Byte;
    //  function ErrorSetting: ByteBool;
    //end;
  end;


  TBreakLocationMap = class(TFpBreakPointMap)
  strict protected type
    TFpBreakPointMapLocDataPairHelper = type helper for TFpBreakPointMapLocDataPair
      function OrigValue: Byte;
      function ErrorSetting: ByteBool;
    end;
  end;

function TBreakLocationMap.TFpBreakPointMapLocDataPairHelper.OrigValue: Byte;
begin
  Result := 0;
end;

function TBreakLocationMap.TFpBreakPointMapLocDataPairHelper.ErrorSetting: ByteBool;
begin
  Result := False;
end;

begin
end.

