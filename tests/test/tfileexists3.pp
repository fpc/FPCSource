begin
  {$if not(fileexists('tfileexists0.pp'))}
    halt(0);
  {$else not(fileexists('tfileexists0.pp'))}
    halt(1);
  {$endif not(fileexists('tfileexists0.pp'))}
end.
