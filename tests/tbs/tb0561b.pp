{ %target=linux }

program setup;

{$mode delphi}{$H+}

Uses    cthreads, Classes, SysUtils, BaseUnix;

{ don't use current directory in case it's on a network share that does not
  support locking
}
Const   Fn      = '/tmp/fpctest.lock';
     F_RDLCK = 0;
     F_WRLCK = 1;
     F_UNLCK = 2;

Var     F, I    : Integer;
     Region  : FLock;
    res: longint;
Begin
  If FileExists (Fn) Then DeleteFile (Fn);
  F := FpOpen (Fn, O_RDWR Or O_CREAT, $1B6);  // $1B6 = o666
  For I := 0 To 255 Do FpWrite (F, I, 1);
  With Region Do                         Begin
    l_type  := F_WRLCK; l_whence := SEEK_SET;
    l_start := 10;      l_len    := 20
  End;
  If FpFcntl (F, F_SETLK, Region) = -1 Then
    begin
      FpClose (F);
      deletefile(fn);
      halt(1);
    end;
  res:=executeprocess('./tb0561a','');
  FpClose (F);
  deletefile(fn);
  if res<>0 then
    halt(2);
End.

