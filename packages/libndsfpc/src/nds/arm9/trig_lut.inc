{$ifdef NDS_INTERFACE}
const
  DEGREES_IN_CIRCLE = (1 shl 15);

function fixedToInt(n, bits: cint): cint; inline;
function intToFixed(n, bits: cint): cint; inline;

function floatToFixed(n, bits: cint): cint; inline;
function fixedToFloat(n, bits: cint): cfloat; inline;

function floorFixed(n, bits: cint): cint; inline;

function degreesToAngle(degrees: cint): cint; inline;
function angleToDegrees(angle: cint): cint; inline;

function cosLerp(angle: cint16): cint16; cdecl; external;
function sinLerp(angle: cint16): cint16; cdecl; external;
function tanLerp(angle: cint16): cint32; cdecl; external;
function acosLerp(par: cint16): cint16; cdecl; external;
function asinLerp(par: cint16): cint16; cdecl; external;

{$endif NDS_INTERFACE}

{$ifdef NDS_IMPLEMENTATION}


function fixedToInt(n, bits: cint): cint; inline;
begin
  result := cint(n shr bits);
end;

function intToFixed(n, bits: cint): cint; inline;
begin
  result := cint(n shl bits);
end;

function floatToFixed(n, bits: cint): cint; inline;
begin
  result := cint(n * cfloat(1 shl bits));
end;

function fixedToFloat(n, bits: cint): cfloat; inline;
begin
  result := cfloat(n) / cfloat(1 shl bits);
end;

function floorFixed(n, bits: cint): cint; inline;
begin
  result := cint(n and (not ((1 shl bits) - 1)));
end;

function degreesToAngle(degrees: cint): cint; inline;
begin
  result := degrees * DEGREES_IN_CIRCLE div 360;
end;

function angleToDegrees(angle: cint): cint; inline;
begin
  result := angle * 360 div DEGREES_IN_CIRCLE;
end;

{$endif NDS_IMPLEMENTATION}
