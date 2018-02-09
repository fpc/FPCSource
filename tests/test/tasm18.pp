{ %NORUN }
{ %CPU=i8086 }
program tasm18;

const
  cval = 1;

type
  foo = record
    b1: byte;
    w: word;
    b2: byte;
  end;
  foo2 = record
    bb1, bb2: byte;
  end;

procedure x; assembler;
asm
  test [di+foo2], cval                   { test word ptr [di], 1   }
  test byte ptr [di+foo], cval           { test byte ptr [di], 1   }
  test [di+foo.w], cval                  { test word ptr [di+1], 1 }
  test [di+foo.b2], cval                 { test byte ptr [di+3], 1 }
  test [di-foo.w], cval                  { test word ptr [di-1], 1 }
  test [di-foo.b2], cval                 { test byte ptr [di-3], 1 }
  test [di+foo.b2+foo.w], cval           { test word ptr [di+4], 1 }
  test [di+foo.w+foo.b2], cval           { test byte ptr [di+4], 1 }

  test byte ptr [di+foo2], cval          { test byte ptr [di], 1   }
  test byte ptr [di+foo], cval           { test byte ptr [di], 1   }
  test byte ptr [di+foo.w], cval         { test byte ptr [di+1], 1 }
  test byte ptr [di+foo.b2], cval        { test byte ptr [di+3], 1 }
  test byte ptr [di-foo.w], cval         { test byte ptr [di-1], 1 }
  test byte ptr [di-foo.b2], cval        { test byte ptr [di-3], 1 }
  test byte ptr [di+foo.b2+foo.w], cval  { test byte ptr [di+4], 1 }
  test byte ptr [di+foo.w+foo.b2], cval  { test byte ptr [di+4], 1 }

  test word ptr [di+foo2], cval          { test word ptr [di], 1   }
  test word ptr [di+foo], cval           { test word ptr [di], 1   }
  test word ptr [di+foo.w], cval         { test word ptr [di+1], 1 }
  test word ptr [di+foo.b2], cval        { test word ptr [di+3], 1 }
  test word ptr [di-foo.w], cval         { test word ptr [di-1], 1 }
  test word ptr [di-foo.b2], cval        { test word ptr [di-3], 1 }
  test word ptr [di+foo.b2+foo.w], cval  { test word ptr [di+4], 1 }
  test word ptr [di+foo.w+foo.b2], cval  { test word ptr [di+4], 1 }
end;

begin
  x;
end.
