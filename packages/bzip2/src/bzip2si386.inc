{$ASMMODE intel}

{$define HAVE_DETRANSFORM}

procedure TDecompressBzip2Stream.detransform;assembler;

asm
{  mov edx,offset c
  call mcount}
  push ebx
  push edi
  xor edx,edx
  lea ebx,[eax+TDecompressBzip2Stream.cftab]
  mov ecx,[eax+TDecompressBzip2Stream.tt_count]
  push esi
  push ebp
  mov esi,[eax+TDecompressBzip2Stream.tt]
  mov edi,esi
  lea ebp,[4*ecx+esi]
  jmp @a2
@a1:
  movzx eax,byte [esi]
  mov ecx,[ebx+4*eax]
  inc dword [ebx+4*eax]
  or [edi+4*ecx],edx
  add edx,$100
  add esi,4
@a2:
  cmp esi,ebp
  jne @a1
  pop ebp
  pop esi
  pop edi
  pop ebx
end ['eax','ecx','edx'];
