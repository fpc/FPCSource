{ example program to call 
	software interrupts
        realintr()
	trealregs type
}

{ Executes a real mode software interrupt

  Exactly the interrupt call to get the DOS version.

  get DOS version Int 21h / function 30h
  Input: AH = $30
         AL = $1
  Return: AL = major version number
          AH = minor version number


}

uses go32; { realintr, trealregs }

var r : trealregs;

begin
     { get DOS version Int 21h / function 30h
       Input: AH = $30
              AL = $1
       Return: AL = major version number
               AH = minor version number
     }
     r.ah := $30;
     r.al := $01;
     realintr($21, r);
     Writeln('DOS v', r.al,'.',r.ah, ' detected');
end.