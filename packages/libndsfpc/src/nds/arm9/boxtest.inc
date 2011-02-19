{$ifdef NDS_INTERFACE}
function BoxTest(x, y, z, width, height, depth: v16): cint; cdecl; external;

function BoxTestf(x, y, z, width, height, depth: cfloat): cint; cdecl; external;

procedure BoxTest_Asynch(x, y, z, width, height, depth: v16); cdecl; external;

procedure BoxTestf_Asynch(x, y, z, width, height, depth: cfloat); cdecl; external;

function BoxTestResult(): cint; cdecl; external;
{$endif NDS_INTERFACE}
