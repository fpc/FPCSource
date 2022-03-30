{ %fail }
{ %target=linux,win32,win64,embedded,freebsd,darwin }
program test;
var
  execBase: pointer;

  procedure test_a_syscall; syscall execBase 6;

begin
  writeln('syscall test');
end.
