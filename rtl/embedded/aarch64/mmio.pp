unit mmio;

interface

procedure DUMMY(Count: DWord);
procedure PUT32(Address: DWord; Value: DWord); inline;
function GET32(Address: DWord) : DWord; inline;

implementation

procedure DUMMY(Count: DWord);
var
    i : DWord;
begin
    for i := 0 to Count do
    begin
        asm
            nop
        end;
    end;
end;

procedure PUT32(Address: DWord; Value: DWord); inline;
VAR
    p: ^DWord;
begin
    p := POINTER (Address);
    p^ := Value;
end;

function GET32(Address: DWord) : DWord; inline;
VAR
    p: ^DWord;
begin
    p := POINTER (Address);
    GET32 := p^;
end;

procedure PUT64(Address: QWord; Value: QWord); inline;
VAR
    p: ^QWord;
begin
    p := POINTER (Address);
    p^ := Value;
end;

function GET64(Address: QWord) : QWord; inline;
VAR
    p: ^QWord;
begin
    p := POINTER (Address);
    GET64 := p^;
end;

end.