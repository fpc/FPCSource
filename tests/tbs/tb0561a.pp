{ %norun }
{ %target=linux }

program test;

{$mode delphi}{$H+}

Uses    cthreads, Classes, SysUtils, BaseUnix;

Const   Fn      = '/tmp/fpctest.lock';
     F_RDLCK = 0;
     F_WRLCK = 1;
     F_UNLCK = 2;

Var     F, I    : Integer;
     Region  : FLock;

Begin
  F := FpOpen (Fn, O_RDWR Or O_CREAT, $1B6);   // $1B6 = o666
  With Region Do                               Begin
     l_type  := F_RDLCK; l_whence := SEEK_SET;
     l_start := 80;      l_len    := 1
  End;
  If FpFcntl (F, F_SETLK, Region) = -1 Then
    begin
      writeln(fpgeterrno);
      WriteLn ('unable to apply readlock on 80');   // <-- Error
      halt(1);
    end;
  FpClose (F);
End.
