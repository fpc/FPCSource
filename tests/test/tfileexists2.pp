begin
  {$if fileexists('tfileexists0.pp')}
    halt(1);
  {$else fileexists('tfileexists0.pp')}
    halt(0);
  {$endif fileexists('tfileexists0.pp')}
end.
