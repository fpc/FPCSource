Program bug0185;

{shows some bugs with rangechecks}

var s: String;
    i: integer;
    code: word;
    e: 0..10;

Begin
{$R-}
  s := '$fffff';
  val(s, i, code); {no range check error may occur here}
  Writeln('Integer($fffff) = ',i);

  Write('Enter the value 20 (should not give a rangecheck error): ');
  Readln(e);
{$R+}
  s := '$ffff';
  val(s, i, code); {no range check error may occur here}
  Writeln('integer($ffff) = ', i,'(should not give range check error)');

  Writeln('Enter value from 0-10 to test Val rangecheck, another for subrange rangecheck: ');
  Readln(e);

  Writeln('If you entered a value different from 0-10, subrange range checks don''t work!');
  s := '65535';
  val(s, i, code); {must give a range check error}
  Writeln('Val range check failed!');

End.
