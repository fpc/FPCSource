{ %norun }

program a;
{$ifdef FPC}
	{$mode delphi}
{$endif}

type
    TBase=class
    private
	fData:string;
	procedure Setdata(ndx:integer;const s:string);
	function GetData(ndx:integer):string;
	function OldIsStored(ndx:integer):boolean;
    public
	property Data:string index 0 read GetData write SetData stored OldIsStored; 
    end;
    
    TDerived=class(TBase)
    private
	function IsDataStored(ndx:integer):boolean;
    published
	property Data stored IsDataStored;
    end;
    
    
    procedure TBase.Setdata(ndx:integer;const s:string);
    begin
	if ndx=0 then fData:=s;
    end;
    
    function TBase.GetData(ndx:integer):string;
    begin
	if ndx=0 then 
	    Result:=fData
	else
	    Result:='';
    end;
    
    function TBase.OldIsStored(ndx:integer):boolean;
    begin
	Result:=ndx>1;
    end;
    
    
    
    function TDerived.IsDataStored(ndx:integer):boolean;
    begin
	Result:=ndx=0;
    end;
    
    
begin
end.
