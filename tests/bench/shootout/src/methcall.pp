{ Method Calls }

program methcall;


uses SysUtils;

type TToggle = class
    private
        value : boolean;

    public
        property Bool : boolean read value write value;
        procedure Activate;
end;

type TNthToggle = class
    constructor Create;
    private
        value : boolean;
        counter : integer;
        cmax : integer;
    public
        property CountMax : integer read cmax write cmax;
        property Bool : boolean read value write value;
        procedure Activate;
end;

constructor TNthToggle.Create;
begin
    counter := 0;
end;

procedure TToggle.Activate;
begin
    if value = True Then
        value := False
    else
        value := True;
end;

procedure TNthToggle.Activate;
begin
    counter := counter + 1;
    if counter >= cmax Then begin
        if value = True Then
            value := False
        Else
            value := True;
        counter := 0;
    end;
end;


var
    NUM, i : longint;
    val : boolean;
    oToggle : TToggle;
    onToggle : TNthToggle;
begin
    if ParamCount = 0 then
        NUM := 1
    else
        NUM := StrToInt(ParamStr(1));

    if NUM < 1 then NUM := 1;

    val := True;
    oToggle := TToggle.Create;
    oToggle.Bool := val;
    For i := 1 to NUM do
    begin
        oToggle.Activate;
        val := oToggle.Bool;
    end;
    If val = True Then
        WriteLn('true')
    else
        WriteLn('false');

    val := True;
    onToggle := TNthToggle.Create;
    onToggle.Bool := val;
    onToggle.CountMax := 3;
    For i := 1 to NUM do
    begin
        onToggle.Activate;
        val := onToggle.Bool;
    end;
    If val = True Then
        WriteLn('true')
    else
        WriteLn('false');
end.
