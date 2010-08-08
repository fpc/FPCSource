{ %fail }
{ Old file: tbs0229.pp }
{ consts > 255 are truncated (should work in -S2,-Sd)  OK 0.99.11 (PFV) }
{ this is not true anymore because it can lead silently to bugs,
  it is allowed now in $H+ mode else it causes an error (FK) }

{$mode objfpc}
{$X-}

const
   CRLF = #13#10;
   c =
        '1-----------------'+CRLF+
        '2/PcbDict 200 dict'+CRLF+
        '3PcbDicljkljkljk b'+CRLF+
        '4PcbDict /DictMaix'+CRLF+
        '5% draw a pin-poll'+CRLF+
        '6% get x+CRLF+ y s'+CRLF+
        '7/thickness exch h'+CRLF+
        '8gsave x y transls'+CRLF+
        '9---------jljkljkl'+crlf+
        '10----------2jkljk'+crlf+
        '11----------jkllkk'+crlf+
        'eeeeeeeeeeeeeeeeee'+crlf+
        '2-----------------'+CRLF+
        '2/PcbDict 200 dice'+CRLF+
        'END____.XXXXXxjk b'+CRLF+
        '4PcbDict /DictMaix'+CRLF+
        '5% draw a pin-poll'+CRLF+
        '6% get x+CRLF+ y s'+CRLF+
        '7/thickness exch h'+CRLF+
        '8gsave x y transls'+CRLF+
        '9---------jljkljkl'+crlf+
        '10----------2jkljk'+crlf+
        '11----------jkllkk'+crlf+
        'eeeeeeeeeeeeeeeeee12';

begin
   write(c);
end.
