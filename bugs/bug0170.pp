begin
asm
	call {$ifdef dummy}freemem{$else}fpc_freemem{$endif}
end;
	
end.
