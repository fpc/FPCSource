{ %version=1.1 }
{ %TARGET=linux,freebsd,darwin,solaris }

{$linklib c}

type
  tprintfproc=procedure(t:pchar);varargs;cdecl;

procedure printf(t:pchar);varargs;cdecl;external;

var
  t : tprintfproc;
begin
  printf('Proc test %d %s %lf'#10,1,'test',1234.5678);

  t:=@printf;
  t('Procvar test %d %s %lf'#10,2,'test',1234.5678);
end.
