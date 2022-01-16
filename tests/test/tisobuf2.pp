{ %OPT=-Sr }
{$mode iso}
program test(input, output, testfile);

  var
    testfile : text;
    s : array[1..100] of char;
  begin
    rewrite(testfile);
    writeln(testfile,'Hello world');
    close(testfile);

    assign(testfile,'TESTFILE.txt');
    reset(testfile);
    readln(testfile,s);
    if (s[1]<>'H') or (s[2]<>'e') then
      halt(1);
    close(testfile);
    erase(testfile);

    writeln('ok');
  end.


