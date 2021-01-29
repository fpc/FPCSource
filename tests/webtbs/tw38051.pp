{ %NORUN }

program tw38051;

const cChr = chr(12); { Ok }

type Tcha = ord(3)..ord(12); { Ok }
type Tchz = #3..#12; { Ok }
type Tcho = char(3)..char(12);{ Ok }
type Tchr = chr(3)..chr(12); { Error: Identifier not found "chr" }

type TArrChr = array [chr(3)..chr(12)] of char; { Ok }

var cz : #3..#12; { Ok }
var ch : chr(3)..chr(12); { Error: Identifier not found "chr" }


var c : char;
begin
     c:=chr(12); { Ok }
end.
