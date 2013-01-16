
// general purpose unix routines for syscall based *nix system unit.

{$ifndef FPC_USE_LIBC}
Function fpgetenv(name:pchar):pchar;[public, alias : 'FPC_SYSC_FPGETENVPCHAR'];

var
  p     : ppchar;
  found : boolean;
  np,cp : pchar;
  len,i : longint;
Begin
  if (name=nil) or (envp=NIL) Then
   exit(NIL);
  np:=name;
  while (np^<>#0) and (np^<>'=') DO
   inc(np);
  len:=np-name;
  p:=envp;
  while (p^<>NIL) DO
   Begin
    cp:=p^;
    np:=name;
    i:=len;
    while (i<>0) and (cp^<>#0) DO
      Begin
        if cp^<>np^ Then
          Begin
           inc(cp); inc(np);
           break;
         End;
        inc(cp); inc(np);
        dec(i)
      End;
    if (i=0) and (cp^='=') Then
        exit(cp+1);
   inc(p);
  end;
 fpgetenv:=nil;
End;
{$ENDIF}

