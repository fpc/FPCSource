{ Source provided for Free Pascal Bug Report 2916 }
{ Submitted by "Ivo Steinmann" on  2004-01-24 }
{ e-mail: isteinmann@bluewin.ch }

var
  x :longint;
begin
{$IF Defined(fpc)}
  writeln('do_foo');
  inc(x);
{$ELSEIF Defined(fpc)}
  writeln('do_bar');
{$ELSEIF Defined(fpc)}
  writeln('fpc');
{$ELSE}
  writeln('else');
{$IFEND}

{$IF Defined(fpc1)}
  writeln('do_foo');
{$ELSEIF Defined(fpc)}
  writeln('do_bar');
  inc(x);
{$ELSEIF Defined(fpc)}
  writeln('fpc');
{$ELSE}
  writeln('else');
{$IFEND}

{$IF Defined(fpc1)}
  writeln('do_foo');
{$ELSEIF Defined(fpc1)}
  writeln('do_bar');
{$ELSEIF Defined(fpc)}
  writeln('fpc');
  inc(x);
{$ELSE}
  writeln('else');
{$IFEND}

{$IF Defined(fpc1)}
  writeln('do_foo');
{$ELSEIF Defined(fpc1)}
  writeln('do_bar');
{$ELSEIF Defined(fpc1)}
  writeln('fpc');
{$ELSE}
  writeln('else');
  inc(x);
{$IFEND}

  if x<>4 then
    writeln('ERROR!');
end.
