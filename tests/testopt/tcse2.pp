{ %OPT=-OG2}
{$r+}

type
  tsubr = 1..100000;
  tarr = array[1..100000] of longint;

function test(b: tsubr): longint;
begin
 test := b;
end;

var
  p: ^longint;
  l: longint;
  a, a2: tarr;

begin
  getmem(p,4);
  p^ := 100000;
  l := 5;
  { clear the optimizer state }
  asm
  end;
{$r-}
  { get p^ in eax, the following statement generates the code }
  {   movl A,%eax                                             }
  {   movl (%eax),%eax                                        }
  a[p^] := l;
{$r+}
  { now, p^ gets rangechecked, this generates the code                  }
  {   movl A,%eax        (1)                                            }
  {   movl (%eax),%ecx   (1)                                            }
  {   ...                                                               }
  {   call rangecheck_procedure                                         }
  {   pushl (%eax)                                                      }
  {                                                                     }
  { With the bug in the optimizer, the instructions marked with (1) are }
  { replaced by                                                         }
  {   movl %eax,%ecx                                                    }
  {                                                                     }
  { and as such the "pushl (%eax)" pushes a wrong value afterwards      }
  l := test(p^);
  if l <> 100000 then
    begin
      writeln('Problem 1!');
      halt(1);
    end;
  p^ := 5;
  l := 5;
  { clear the optimizer state }
  asm
  end;
{$r-}
  { the following moves p^ in %edx }
  a2[l] := a[p^];
{$r+}
  { same test as before, but now the original value comes from edx }
  { instead of that it is already in eax (so check that it doesn't }
  { replace the                                                    }
  {   movl P,%eax                                                  }
  {   movl (%eax),%ecx                                             }
  { with                                                           }
  {   movl %edx,%ecx                                               }
  l := test(p^);
  if l <> 5 then
    begin
      writeln('Problem 2!');
      halt(1);
    end;
  freemem(p,4);
end.
