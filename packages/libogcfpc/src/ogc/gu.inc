{$IFDEF OGC_INTERFACE}
{$ifdef GEKKO}
{$define MTX_USE_PS}
{$undef MTX_USE_C}
{$endif GEKKO}

{$ifndef GEKKO}
{$define MTX_USE_C}
{$undef MTX_USE_PS}
{$endif GEKKO}

//#ifndef MAX
//#define MAX(a,b) (((a)>(b))?(a):(b))
//#endif
//#ifndef MIN
//#define MIN(a,b) (((a)<(b))?(a):(b))
//#endif

const
  M_PI = 3.14159265358979323846;
  M_DTOR = 3.14159265358979323846  / 180.0;

function FTOFIX32(x: cfloat): f32; inline;
function FIX32TOF(x: f32): cfloat; inline;
function FTOFRAC8(x: cfloat): cint; inline;
function DegToRad(a: f32): f32; inline;
function RadToDeg(a: f32): f32; inline;

type
  _vecf = record
    x : f32;
    y : f32;
    z : f32;
  end;
  guVector = _vecf;
  PguVector = ^guVector;

  _qrtn = record
    x : f32;
    y : f32;
    z : f32;
    w : f32;
  end;
  guQuaternion = _qrtn;
  PguQuaternion = ^guQuaternion;

  Mtx = array [0..2, 0..3] of f32;
  Mtxp = array [0..3] of f32;
  PMtxp = ^Mtxp;

  ROMtx = array [0..3, 0..2] of f32;
  ROMtxP = array [0..2] of f32;
  PROMtxP = ^ROMtxP;

  Mtx33 = array [0..2, 0..2] of f32;
  Mtx33P = array [0..2] of f32;
  PMtx33P = ^Mtx33P;

  Mtx44 = array [0..3, 0..3] of f32;
  Mtx44P = array [0..3] of f32;
  PMtx44P = ^Mtx44P;

function guMtxRowCol(mt: Mtx; row, col: cuint8): f32; inline;

procedure guFrustum(mt: Mtx44; t, b, l, r, n, f: f32); cdecl; external;

procedure guPerspective(mt: Mtx44; fovy, aspect, n, f: f32); cdecl; external;


procedure guOrtho(mt: Mtx44; t, b, l, r, n, f: f32); cdecl; external;


procedure guLightPerspective(mt: Mtx;
  fovY, aspect, scaleS, scaleT, transS, transT: f32); cdecl; external;

procedure guLightOrtho(mt: Mtx; t, b, l, r, scaleS, scaleT, transS, transT: f32); cdecl; external;


procedure guLightFrustum(mt: Mtx;
  t, b, l, r, n, scaleS, scaleT, transS, transT: f32); cdecl; external;

procedure guLookAt(mt: Mtx; camPos, camUp, target: PguVector); cdecl; external;

procedure guVecHalfAngle(a, b, half: PguVector); cdecl; external;


procedure c_guVecAdd(a, b, ab: PguVector); cdecl; external;
procedure c_guVecSub(a, b, ab: PguVector); cdecl; external;
procedure c_guVecScale(src, dst: PguVector; scale: f32); cdecl; external;
procedure c_guVecNormalize(v: PguVector); cdecl; external;
procedure c_guVecMultiply(mt: Mtx; src, dst: PguVector); cdecl; external;
procedure c_guVecCross(a, b, axb: PguVector); cdecl; external;
procedure c_guVecMultiplySR(mt: Mtx; src, dst: PguVector); cdecl; external;
function c_guVecDotProduct(a, b: PguVector): f32; cdecl; external;


{$ifdef GEKKO}
procedure ps_guVecAdd(a, b, ab: PguVector); cdecl; external; register;
procedure ps_guVecSub(a, b, ab: PguVector); cdecl; external; register;
procedure ps_guVecScale(src, dst: PguVector; scale: f32); cdecl; external; register;
procedure ps_guVecNormalize(v: PguVector); cdecl; external; register;
procedure ps_guVecCross(a, b, axb: PguVector); cdecl; external; register;
procedure ps_guVecMultiply(mt: Mtx; src, dst: PguVector); cdecl; external; register;
procedure ps_guVecMultiplySR(mt: Mtx; src, dst: PguVector); cdecl; external; register;
function ps_guVecDotProduct(a, b: PguVector): f32; cdecl; external; register;
{$endif GEKKO}

procedure c_guQuatAdd(a, b, ab: PguQuaternion); cdecl; external;
procedure c_guQuatSub(a, b, ab: PguQuaternion); cdecl; external;
procedure c_guQuatMultiply(a, b, ab: PguQuaternion); cdecl; external;
procedure c_guQuatNormalize(a, d: PguQuaternion); cdecl; external;
procedure c_guQuatInverse(a, d: PguQuaternion); cdecl; external;
procedure c_guQuatMtx(a: PguQuaternion; m: Mtx); cdecl; external;


{$ifdef GEKKO}
procedure ps_guQuatAdd(a, b, ab: PguQuaternion); cdecl; external; register;
procedure ps_guQuatSub(a, b, ab: PguQuaternion); cdecl; external; register;
procedure ps_guQuatMultiply(a, b, ab: PguQuaternion); cdecl; external; register;
procedure ps_guQuatNormalize(a, d: PguQuaternion); cdecl; external; register;
procedure ps_guQuatInverse(a, d: PguQuaternion); cdecl; external; register;
{$endif GEKKO}

procedure c_guMtxIdentity(mt: Mtx); cdecl; external;
procedure c_guMtxCopy(src, dst: Mtx); cdecl; external;
procedure c_guMtxConcat(a, b, ab: Mtx); cdecl; external;
procedure c_guMtxScale(mt: Mtx; xS, yS, zS: f32); cdecl; external;
procedure c_guMtxScaleApply(src, dst: Mtx; xS, yS, zS: f32); cdecl; external;
procedure c_guMtxApplyScale(src, dst: Mtx; xS, yS, zS: f32); cdecl; external;
procedure c_guMtxTrans(mt: Mtx; xT, yT, zT: f32); cdecl; external;
procedure c_guMtxTransApply(src, dst: Mtx; xT, yT, zT: f32); cdecl; external;
procedure c_guMtxApplyTrans(src, dst: Mtx; xT, yT, zT: f32); cdecl; external;
function c_guMtxInverse(src, inv: Mtx): cuint32; cdecl; external;
function c_guMtxInvXpose(src, xPose: Mtx): cuint32; cdecl; external;
procedure c_guMtxTranspose(src, xPose: Mtx); cdecl; external;
procedure c_guMtxRotRad(mt: Mtx; const axis: cchar; rad: f32); cdecl; external;
procedure c_guMtxRotTrig(mt: Mtx; const axis: cchar; sinA, cosA: f32); cdecl; external;
procedure c_guMtxRotAxisRad(mt: Mtx; axis: PguVector; rad: f32); cdecl; external;
procedure c_guMtxReflect(m: Mtx; p, n: PguVector); cdecl; external;
procedure c_guMtxQuat(m: Mtx; a: PguQuaternion); cdecl; external;


{$ifdef GEKKO}
procedure ps_guMtxIdentity(mt: Mtx); cdecl; external; register;
procedure ps_guMtxCopy(src, dst: Mtx); cdecl; external; register;
procedure ps_guMtxConcat(a, b, ab: Mtx); cdecl; external; register;
procedure ps_guMtxTranspose(src, xPose: Mtx); cdecl; external; register;
function ps_guMtxInverse(src, inv: Mtx): cuint32; cdecl; external; register;
function ps_guMtxInvXpose(src, xPose: Mtx): cuint32; cdecl; external; register;
procedure ps_guMtxScale(mt: Mtx; xS, yS, zS: f32); cdecl; external; register;
procedure ps_guMtxScaleApply(src, dst: Mtx; xS, yS, zS: f32); cdecl; external; register;
procedure ps_guMtxApplyScale(src, dst: Mtx; xS, yS, zS: f32); cdecl; external; register;
procedure ps_guMtxTrans(mt: Mtx; xT, yT, zT: f32); cdecl; external; register;
procedure ps_guMtxTransApply(src, dst: Mtx; xT, yT, zT: f32); cdecl; external; register;
procedure ps_guMtxApplyTrans(src, dst: Mtx; xT, yT, zT: f32); cdecl; external; register;
procedure ps_guMtxRotRad(mt: Mtx; axis: char_; rad: f32); cdecl; external; register;
procedure ps_guMtxRotTrig(mt: Mtx; axis: char_; sinA, cosA: f32); cdecl; external; register;
procedure ps_guMtxRotAxisRad(mt: Mtx; axis: PguVector; tmp0: f32); cdecl; external; register;
procedure ps_guMtxReflect(m: Mtx; p, n: PguVector); cdecl; external; register;
{$endif GEKKO}

{$ifdef MTX_USE_C}
procedure guVecAdd(a, b, ab: PguVector); cdecl; external name 'c_guVecAdd';
procedure guVecSub(a, b, ab: PguVector); cdecl; external name 'c_guVecSub';
procedure guVecScale(src, dst: PguVector; scale: f32); cdecl; external name 'c_guVecScale';
procedure guVecNormalize(v: PguVector); cdecl; external name 'c_guVecNormalize';
procedure guVecMultiply(mt: Mtx; src, dst: PguVector); cdecl; external name 'c_guVecMultiply';
procedure guVecCross(a, b, axb: PguVector); cdecl; external name 'c_guVecCross';
procedure guVecMultiplySR(mt: Mtx; src, dst: PguVector); cdecl; external name 'c_guVecMultiplySR';
function guVecDotProduct(a, b: PguVector): f32; cdecl; external name 'c_guVecDotProduct';

procedure guQuatAdd(a, b, ab: PguQuaternion); cdecl; external name 'c_guQuatAdd';
procedure guQuatSub(a, b, ab: PguQuaternion); cdecl; external name 'c_guQuatSub';
procedure guQuatMultiply(a, b, ab: PguQuaternion); cdecl; external name 'c_guQuatMultiply';
procedure guQuatNormalize(a, d: PguQuaternion); cdecl; external name 'c_guQuatNormalize';
procedure guQuatInverse(a, d: PguQuaternion); cdecl; external name 'c_guQuatInverse';
procedure guQuatMtx(a: PguQuaternion; m: Mtx); cdecl; external name 'c_guQuatMtx';

procedure guMtxIdentity(mt: Mtx); cdecl; external name 'c_guMtxIdentity';
procedure guMtxCopy(src, dst: Mtx); cdecl; external name 'c_guMtxCopy';
procedure guMtxConcat(a, b, ab: Mtx); cdecl; external name 'c_guMtxConcat';
procedure guMtxScale(mt: Mtx; xS, yS, zS: f32); cdecl; external name 'c_guMtxScale';
procedure guMtxScaleApply(src, dst: Mtx; xS, yS, zS: f32); cdecl; external name 'c_guMtxScaleApply';
procedure guMtxApplyScale(src, dst: Mtx; xS, yS, zS: f32); cdecl; external name 'c_guMtxApplyScale';
procedure guMtxTrans(mt: Mtx; xT, yT, zT: f32); cdecl; external name 'c_guMtxTrans';
procedure guMtxTransApply(src, dst: Mtx; xT, yT, zT: f32); cdecl; external name 'c_guMtxTransApply';
procedure guMtxApplyTrans(src, dst: Mtx; xT, yT, zT: f32); cdecl; external name 'c_guMtxApplyTrans';
function guMtxInverse(src, inv: Mtx): cuint32; cdecl; external name 'c_guMtxInverse';
function guMtxInvXpose(src, xPose: Mtx): cuint32; cdecl; external name 'c_guMtxInvXpose';
procedure guMtxTranspose(src, xPose: Mtx); cdecl; external name 'c_guMtxTranspose';
procedure guMtxRotRad(mt: Mtx; const axis: cchar; rad: f32); cdecl; external name 'c_guMtxRotRad';
procedure guMtxRotTrig(mt: Mtx; const axis: cchar; sinA, cosA: f32); cdecl; external name 'c_guMtxRotTrig';
procedure guMtxRotAxisRad(mt: Mtx; axis: PguVector; rad: f32); cdecl; external name 'c_guMtxRotAxisRad';
procedure guMtxReflect(m: Mtx; p, n: PguVector); cdecl; external name 'c_guMtxReflect';
procedure guMtxQuat(m: Mtx; a: PguQuaternion); cdecl; external name 'c_guMtxQuat';


{$else}

procedure guVecAdd(a, b, ab: PguVector); cdecl; external name 'ps_guVecAdd'; register;
procedure guVecSub(a, b, ab: PguVector); cdecl; external name 'ps_guVecSub'; register;
procedure guVecScale(src, dst: PguVector; scale: f32); cdecl; external name 'ps_guVecScale'; register;
procedure guVecNormalize(v: PguVector); cdecl; external name 'ps_guVecNormalize'; register;
procedure guVecCross(a, b, axb: PguVector); cdecl; external name 'ps_guVecCross'; register;
procedure guVecMultiply(mt: Mtx; src, dst: PguVector); cdecl; external name 'ps_guVecMultiply'; register;
procedure guVecMultiplySR(mt: Mtx; src, dst: PguVector); cdecl; external name 'ps_guVecMultiplySR'; register;
function guVecDotProduct(a, b: PguVector): f32; cdecl; external name 'ps_guVecDotProduct'; register;

procedure guQuatAdd(a, b, ab: PguQuaternion); cdecl; external name 'ps_guQuatAdd'; register;
procedure guQuatSub(a, b, ab: PguQuaternion); cdecl; external name 'ps_guQuatSub'; register;
procedure guQuatMultiply(a, b, ab: PguQuaternion); cdecl; external name 'ps_guQuatMultiply'; register;
procedure guQuatNormalize(a, d: PguQuaternion); cdecl; external name 'ps_guQuatNormalize'; register;
procedure guQuatInverse(a, d: PguQuaternion); cdecl; external name 'ps_guQuatInverse'; register;


procedure guMtxIdentity(mt: Mtx); cdecl; external name 'ps_guMtxIdentity'; register;
procedure guMtxCopy(src, dst: Mtx); cdecl; external name 'ps_guMtxCopy'; register;
procedure guMtxConcat(a, b, ab: Mtx); cdecl; external name 'ps_guMtxConcat'; register;
procedure guMtxTranspose(src, xPose: Mtx); cdecl; external name 'ps_guMtxTranspose'; register;
function guMtxInverse(src, inv: Mtx): cuint32; cdecl; external name 'ps_guMtxInverse'; register;
function guMtxInvXpose(src, xPose: Mtx): cuint32; cdecl; external name 'ps_guMtxInvXpose'; register;
procedure guMtxScale(mt: Mtx; xS, yS, zS: f32); cdecl; external name 'ps_guMtxScale'; register;
procedure guMtxScaleApply(src, dst: Mtx; xS, yS, zS: f32); cdecl; external name 'ps_guMtxScaleApply'; register;
procedure guMtxApplyScale(src, dst: Mtx; xS, yS, zS: f32); cdecl; external name 'ps_guMtxApplyScale'; register;
procedure guMtxTrans(mt: Mtx; xT, yT, zT: f32); cdecl; external name 'ps_guMtxTrans'; register;
procedure guMtxTransApply(src, dst: Mtx; xT, yT, zT: f32); cdecl; external name 'ps_guMtxTransApply'; register;
procedure guMtxApplyTrans(src, dst: Mtx; xT, yT, zT: f32); cdecl; external name 'ps_guMtxApplyTrans'; register;
procedure guMtxRotRad(mt: Mtx; axis: char_; rad: f32); cdecl; external name 'ps_guMtxRotRad'; register;
procedure guMtxRotTrig(mt: Mtx; axis: char_; sinA, cosA: f32); cdecl; external name 'ps_guMtxRotTrig'; register;
procedure guMtxRotAxisRad(mt: Mtx; axis: PguVector; tmp0: f32); cdecl; external name 'ps_guMtxRotAxisRad'; register;
procedure guMtxReflect(m: Mtx; p, n: PguVector); cdecl; external name 'ps_guMtxReflect'; register;
{$endif}

procedure guMtxRotDeg(mt: Mtx; axis: cchar; deg: f32); inline;
procedure guMtxRotAxisDeg(mt: Mtx; axis: pguVector; deg: f32); inline;
{$ENDIF}

{$IFDEF OGC_IMPLEMENTATION}

function FTOFIX32(x: cfloat): f32; inline;
begin
  FTOFIX32 := cint32(x * f32($00010000));
end;

function FIX32TOF(x: f32): cfloat; inline;
begin
  FIX32TOF := f32(x) * (1.0 / f32($00010000));
end;

function FTOFRAC8(x: cfloat): cint; inline;
begin
  FTOFRAC8 := cint32(MIN(x * 128.0, 127.0)) and $ff;
end;

function DegToRad(a: f32): f32; inline;
begin
  DegToRad := f32(a *  0.01745329252);
end;

function RadToDeg(a: f32): f32; inline;
begin
  RadToDeg := a * 57.29577951;
end;

function guMtxRowCol(mt: Mtx; row, col: cuint8): f32; inline;
begin
  guMtxRowCol := mt[row, col];
end;

procedure guMtxRotDeg(mt: Mtx; axis: cchar; deg: f32); inline;
begin
  guMtxRotRad(mt, axis, DegToRad(deg));
end;

procedure guMtxRotAxisDeg(mt: Mtx; axis: pguVector; deg: f32); inline;
begin
  guMtxRotAxisRad(mt, axis, DegToRad(deg));
end;

{$ENDIF}
