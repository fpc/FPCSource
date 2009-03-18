{ %interactive }

{$inline on}
uses
  ctypes;
const
  MATRIX_TRANSLATE      : pcint32 = pointer($04000470);

function floattof32(n: cfloat): cint32; inline;
  begin
    floattof32 := cint32(n);
  end;

procedure glTranslate3f32({ x, y,} z: cint32); inline; 
  begin 
    MATRIX_TRANSLATE^ := z;
  end;

begin
    { check that the inlined version of glTranslate3f32 does *NOT* perform
      an unaligned store (i.e., make sure it performs one 4 byte store
      rather than 4 one byte stores on platforms that require aligned memory
      accesses)
    }
    glTranslate3f32(floattof32(-1.0));
end.
