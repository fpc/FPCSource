{ %CPU=i386,x86_64 }
{ %norun }

const
{$ifdef CPUX86_64}
  expected_code : array[0..44] of byte = ($f2,$f0,$87,$00,
                                          $f3,$f0,$87,$00,
                                          $c7,$f8,$00,$00,$00,$00,
                                          $c6,$f8,$01,
                                          $0f,$01,$d5,
                                          $0f,$01,$d6,
                                          $66,$0f,$c7,$f0,
                                          $0f,$c7,$f3,
                                          $49,$0f,$c7,$f4,
                                          $66,$0f,$c7,$f8,
                                          $0f,$c7,$fb,
                                          $49,$0f,$c7,$fc);
{$else CPUX86_64}
  expected_code : array[0..36] of byte = ($f2,$f0,$87,$00,
                                          $f3,$f0,$87,$00,
                                          $c7,$f8,$00,$00,$00,$00,
                                          $c6,$f8,$01,
                                          $0f,$01,$d5,
                                          $0f,$01,$d6,
                                          $66,$0f,$c7,$f0,
                                          $0f,$c7,$f3,
                                          $66,$0f,$c7,$f8,
                                          $0f,$c7,$fb);

{$endif CPUX86_64}

procedure proc;assembler;nostackframe;
  asm
{$ifdef CPUX86_64}
    xacquire lock xchgl (%rax),%eax
    xrelease lock xchgl (%rax),%eax
{$else CPUX86_64}
    xacquire lock xchgl (%eax),%eax
    xrelease lock xchgl (%eax),%eax
{$endif CPUX86_64}
    xbegin .L1
.L1:
    xabort $1
    xend
    xtest
    rdrand  %ax
    rdrand  %ebx
{$ifdef CPUX86_64}
    rdrand  %r12
{$endif CPUX86_64}
    rdseed  %ax
    rdseed  %ebx
{$ifdef CPUX86_64}
    rdseed  %r12
{$endif CPUX86_64}
   end;


var
  P : pointer;
  i : integer;

begin
  for i:=0 to high(expected_code) do
    if (pbyte(@proc)+i)^<>expected_code[i] then
      begin
        writeln('Error at pos ',i,'. Expected $',hexstr(expected_code[i],2),' got $',hexstr((pbyte(@proc)+i)^,2));
        halt(1);
      end;
  writeln('ok');
end.
