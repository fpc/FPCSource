begin
  {$if fileexists('tfileexists1.pp')}
    halt(0);
  {$else fileexists('tfileexists1.pp')}
    halt(1);
  {$endif fileexists('tfileexists1.pp')}
end.
