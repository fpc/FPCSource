{$ifdef NDS_IMPLEMENTATION}
function PosTestBusy(): cbool; inline;
begin
  PosTestBusy := (GFX_STATUS^ and (1 shl 0)) <> 0;
end;

procedure PosTest_Asynch(x, y, z: v16); inline;
begin
	GFX_POS_TEST^ := VERTEX_PACK(x, y);
	GFX_POS_TEST^ := z;
end;

procedure PosTest(x, y, z: v16); inline;
begin 
	PosTest_Asynch(x, y, z);
	while PosTestBusy() do;
end;

function PosTestWresult(): cint32; inline;
begin
  PosTestWresult := GFX_POS_RESULT[3];
end;

function PosTestXresult(): cint32; inline;
begin
  PosTestXresult := GFX_POS_RESULT[0];
end;

function PosTestYresult(): cint32; inline;
begin
	PosTestYresult := GFX_POS_RESULT[1];
end;

function PosTestZresult(): cint32; inline;
begin
  PosTestZresult := GFX_POS_RESULT[2];
end;
{$endif NDS_IMPLEMENTATION}

{$ifdef NDS_INTERFACE}
function PosTestBusy(): cbool; inline;
procedure PosTest_Asynch(x, y, z: v16); inline;
procedure PosTest(x, y, z: v16); inline;
function PosTestWresult(): cint32; inline;
function PosTestXresult(): cint32; inline;
function PosTestYresult(): cint32; inline;
function PosTestZresult(): cint32; inline;
{$endif NDS_INTERFACE}
