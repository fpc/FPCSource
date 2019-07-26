{ Parsed from GLKit.framework GLKMathTypes.h }

{$ifndef __GLK_MATH_TYPES_H}

{$if defined(TARGET_OS_MAC) and not defined(TARGET_OS_IPHONE)}

{$ifndef __SSE3__}

{$else}
{$ifdef TYPES}
const
  GLK_SSE3_INTRINSICS = 1;
{$endif}

{$endif}
{$endif}

{$if (defined(__STRICT_ANSI__))}
{$ifdef TYPES}
type
  _GLKMatrix2 = record
    m:array[0..3] of single;
  end;
type
  _GLKMatrix2Ptr = ^_GLKMatrix2;

type
  GLKMatrix2 = _GLKMatrix2;
  GLKMatrix2Ptr = ^GLKMatrix2;
{$endif}

{$else}
{$ifdef TYPES}
type
  _GLKMatrix2 = record
    case longint of
      0: (
        _anonStruct_GLKMathTypes0: record
          m00, m01: single;
          m10, m11: single;
        end;
      );
      1: (m2:array[0..1, 0..1] of single);
      2: (m:array[0..3] of single);
  end;
type
  _GLKMatrix2Ptr = ^_GLKMatrix2;

type
  GLKMatrix2 = _GLKMatrix2;
  GLKMatrix2Ptr = ^GLKMatrix2;
{$endif}

{$endif}

{$if (defined(__STRICT_ANSI__))}
{$ifdef TYPES}
type
  _GLKMatrix3 = record
    m:array[0..8] of single;
  end;
type
  _GLKMatrix3Ptr = ^_GLKMatrix3;

type
  GLKMatrix3 = _GLKMatrix3;
  GLKMatrix3Ptr = ^GLKMatrix3;
{$endif}

{$else}
{$ifdef TYPES}
type
  _GLKMatrix3 = record
    case longint of
      0: (
        _anonStruct_GLKMathTypes1: record
          m00, m01, m02: single;
          m10, m11, m12: single;
          m20, m21, m22: single;
        end;
      );
      1: (m:array[0..8] of single);
  end;
type
  _GLKMatrix3Ptr = ^_GLKMatrix3;

type
  GLKMatrix3 = _GLKMatrix3;
  GLKMatrix3Ptr = ^GLKMatrix3;
{$endif}

{$endif}

{$if (defined(__STRICT_ANSI__))}
{$ifdef TYPES}
type
  _GLKMatrix4 = record
    m:array[0..15] of single;
  end;
type
  _GLKMatrix4Ptr = ^_GLKMatrix4;

type
  GLKMatrix4 = _GLKMatrix4;
  GLKMatrix4Ptr = ^GLKMatrix4;
{$endif}

{$else}
{$ifdef TYPES}
type
  _GLKMatrix4 = record
    case longint of
      0: (
        _anonStruct_GLKMathTypes2: record
          m00, m01, m02, m03: single;
          m10, m11, m12, m13: single;
          m20, m21, m22, m23: single;
          m30, m31, m32, m33: single;
        end;
      );
      1: (m:array[0..15] of single);
  end;
type
  _GLKMatrix4Ptr = ^_GLKMatrix4;

type
  GLKMatrix4 = _GLKMatrix4;
  GLKMatrix4Ptr = ^GLKMatrix4;
{$endif}

{$endif}

{$if (defined(__STRICT_ANSI__))}
{$ifdef TYPES}
type
  _GLKVector2 = record
    v:array[0..1] of single;
  end;
type
  _GLKVector2Ptr = ^_GLKVector2;

type
  GLKVector2 = _GLKVector2;
  GLKVector2Ptr = ^GLKVector2;
{$endif}

{$else}
{$ifdef TYPES}
type
  _GLKVector2 = record
    case longint of
      0: (
        _anonStruct_GLKMathTypes3: record
          x, y: single;
        end;
      );
      1: (
        _anonStruct_GLKMathTypes4: record
          s, t: single;
        end;
      );
      2: (v:array[0..1] of single);
  end;
type
  _GLKVector2Ptr = ^_GLKVector2;

type
  GLKVector2 = _GLKVector2;
  GLKVector2Ptr = ^GLKVector2;
{$endif}

{$endif}

{$if (defined(__STRICT_ANSI__))}
{$ifdef TYPES}
type
  _GLKVector3 = record
    v:array[0..2] of single;
  end;
type
  _GLKVector3Ptr = ^_GLKVector3;

type
  GLKVector3 = _GLKVector3;
  GLKVector3Ptr = ^GLKVector3;
{$endif}

{$else}
{$ifdef TYPES}
type
  _GLKVector3 = record
    case longint of
      0: (
        _anonStruct_GLKMathTypes5: record
          x, y, z: single;
        end;
      );
      1: (
        _anonStruct_GLKMathTypes6: record
          r, g, b: single;
        end;
      );
      2: (
        _anonStruct_GLKMathTypes7: record
          s, t, p: single;
        end;
      );
      3: (v:array[0..2] of single);
  end;
type
  _GLKVector3Ptr = ^_GLKVector3;

type
  GLKVector3 = _GLKVector3;
  GLKVector3Ptr = ^GLKVector3;
{$endif}

{$endif}

{$if (defined(__STRICT_ANSI__))}
{$ifdef TYPES}
type
  _GLKVector4 = record
    v:array[0..3] of single;
  end;
type
  _GLKVector4Ptr = ^_GLKVector4;

type
  GLKVector4 = _GLKVector4;
  GLKVector4Ptr = ^GLKVector4;
{$endif}

{$else}
{$ifdef TYPES}
type
  _GLKVector4 = record
    case longint of
      0: (
        _anonStruct_GLKMathTypes8: record
          x, y, z, w: single;
        end;
      );
      1: (
        _anonStruct_GLKMathTypes9: record
          r, g, b, a: single;
        end;
      );
      2: (
        _anonStruct_GLKMathTypes10: record
          s, t, p, q: single;
        end;
      );
      3: (v:array[0..3] of single);
  end;
type
  _GLKVector4Ptr = ^_GLKVector4;

type
  GLKVector4 = _GLKVector4;
  GLKVector4Ptr = ^GLKVector4;
{$endif}

{$endif}

{$if (defined(__STRICT_ANSI__))}
{$ifdef TYPES}
type
  _GLKQuaternion = record
    q:array[0..3] of single;
  end;
type
  _GLKQuaternionPtr = ^_GLKQuaternion;

type
  GLKQuaternion = _GLKQuaternion;
  GLKQuaternionPtr = ^GLKQuaternion;
{$endif}

{$else}
{$ifdef TYPES}
type
  _GLKQuaternion = record
    case longint of
      0: (
        _anonStruct_GLKMathTypes11: record
          v: GLKVector3;
          s: single;
        end;
      );
      1: (
        _anonStruct_GLKMathTypes12: record
          x, y, z, w: single;
        end;
      );
      2: (q:array[0..3] of single);
  end;
type
  _GLKQuaternionPtr = ^_GLKQuaternion;

type
  GLKQuaternion = _GLKQuaternion;
  GLKQuaternionPtr = ^GLKQuaternion;
{$endif}

{$endif}

{$endif}
