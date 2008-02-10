Program TestPass2;

Uses pwd,grp,baseunix;

Procedure printpchar(fieldname:String;p:pchar);

Begin
  If assigned(p) Then
    Begin
      write(fieldname);
      write(':',' ':15-length(fieldname));
      writeln(p);
    End;
End;

Var p : PPasswd;
  supplementary_gids : array[0..99] Of gid_t;
  i,nrgids: cint;
  pgrp: PGroup;

Begin
  p := fpgetpwnam('marcov');
  If assigned(p) Then
    Begin
      printpchar('pw_name',p^.pw_name);
      printpchar('pw_passwd',p^.pw_passwd);
      writeln('pw_uid:',' ':9,p^.pw_uid);
      writeln('pw_gid:',' ':9,p^.pw_gid);
     {$ifdef BSD}
      printpchar('pw_change',p^.pw_change);
      printpchar('pw_class',p^.pw_class);
     {$endif}
      printpchar('pw_gecos',p^.pw_gecos);
      printpchar('pw_dir',p^.pw_dir);
      printpchar('pw_shell',p^.pw_shell);
     {$ifdef BSD}
      printpchar('pw_expire',p^.pw_expire);
      printpchar('pw_fields',p^.pw_fields);
     {$endif}
    End;
  nrgids := 100;
  If fpgetgrouplist(p^.pw_name,p^.pw_gid,@supplementary_gids,@nrgids)<>-1 Then
    Begin
      Write ('Supplementary groups:');
      For i:=0 To nrgids-1 Do
        Begin
          pgrp := fpgetgrgid(supplementary_gids[i]);
          If assigned(pgrp) Then
            write(pgrp^.gr_name,' (',supplementary_gids[i],') ')
          Else
            write(' ???? (',supplementary_gids[i],') ')
        End;
      writeln;
    End;
  writeln;
End.
