{ %NORUN }

program tw35533;
{$mode delphiunicode}

type
  TPointerHelper = record helper for pointer
    function AsNativeUint: nativeuint;
    function PCharLen: uint32;
  end;

function TPointerHelper.AsNativeUint: nativeuint;
begin
  Result := nativeuint(self);
end;

function TPointerHelper.PCharLen: uint32;
begin
  Result := 5; //- Just here to illustrate the issue.
end;

var
  P: pointer;

begin
  P := @ParamStr(0); //- Just a nonsense pointer.
  Writeln( P.AsNativeUInt );
  Writeln( P.PCharLen );
  Readln;
end.
