{ Old file: tbs0128.pp }
{ problem with ^[                                       OK 0.99.6 (PFV) }

{ ^ followed by a letter must be interpreted differently
  depending on context }

const
   ArrowKeysOrFirstLetter='arrow keys '^]^r^z' or First letter.   ';

begin
   writeln(ord(^)));
end.
