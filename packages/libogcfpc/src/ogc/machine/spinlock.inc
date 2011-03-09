
{$IFDEF OGC_INTERFACE}
type
  spinlock_t = record
    lock: cuint32;
  end;
  pspinlock_t = ^spinlock_t;


  rwlock_t = record
    lock: cuint32;
  end;
  prwlock_t = ^rwlock_t;


const
  SPIN_LOCK_UNLOCKED: spinlock_t =  (lock:0) ;
  RW_LOCK_UNLOCKED: rwlock_t = (lock:0);

procedure spin_lock_init(x: pspinlock_t); inline;
function _test_and_set(atomic: pcuint32): cuint32; inline;
function atomic_inc(_pint: pcuint32): cuint32; inline;
function atomic_dec(_pint: pcuint32): cuint32; inline;
procedure spin_lock(lock: pspinlock_t); inline;
procedure spin_lock_irqsave(lock: pspinlock_t; {register} p_isr_level: pcuint32); inline;
procedure spin_unlock(lock: pspinlock_t); inline;
procedure spin_unlock_irqrestore(lock: pspinlock_t; {register} isr_level: cuint32); inline;
procedure read_lock_init(lp: prwlock_t); inline;
procedure read_lock(rw: prwlock_t); inline;
procedure read_unlock(rw: prwlock_t); inline;
procedure write_lock(rw: prwlock_t); inline;
procedure write_unlock(rw: prwlock_t); inline;
{$ENDIF}


{$IFDEF OGC_IMPLEMENTATION}
procedure spin_lock_init(x: pspinlock_t); inline;
begin
 x^ := SPIN_LOCK_UNLOCKED;
end;

function _test_and_set(atomic: pcuint32): cuint32; inline;
var
  ret: cuint32;
begin
  asm
    .L1:
    lwarx r3,0,r4
    cmpwi   0,r3,0
    bne-    .L2
    stwcx.  r5,0,r4
    bne-    .L1
    isync
    .L2:
  end;
  result := ret;
end;


function atomic_inc(_pint: pcuint32): cuint32; inline;
var
	ret: cuint32;
begin
  asm
		.L1:
      lwarx	r3,0,r4
			addi	r3,r3,1
			stwcx.	r3,0,r4
			bne-	.L1
			isync
  end;
  result := ret;
end;

function atomic_dec(_pint: pcuint32): cuint32; inline;
var
  ret: cuint32;
begin
	asm
		.L1:
      lwarx	r3,0,r4
			addi	r3,r3,-1
			stwcx.	r3,0,r4
			bne-	.L1
			isync
  end;
	result := ret;
end;

procedure spin_lock(lock: pspinlock_t); inline;
var
  tmp: cuint32;
begin
  asm
	  b       .L1
  .L2:
      lwzx    r3,0,r4
      cmpwi   0,r3,0
      bne+    .L2
  .L1:
      lwarx   r3,0,r4
      cmpwi   0,r3,0
      bne-    .L2
      stwcx.  r5,0,r4
      bne-    .L2
      isync
  end;
end;

procedure spin_lock_irqsave(lock: pspinlock_t; {register} p_isr_level: pcuint32); inline;
var
  level: cuint32;
  tmp: cuint32;
begin
	_CPU_ISR_Disable(level);
  asm
    b       .L1
	.L2:
    lwzx    r3,0,r4
		cmpwi   0,r3,0
		bne+    .L2
	.L1:
    lwarx   r3,0,r4
		cmpwi   0,r3,0
		bne-    .L2
		stwcx.  r5,0,r4
		bne-    .L2
		isync
  end;
	p_isr_level^ := level;
end;

procedure spin_unlock(lock: pspinlock_t); inline;
begin
  asm
    eieio
  end;
  lock^.lock := 0;
end;

procedure spin_unlock_irqrestore(lock: pspinlock_t; {register} isr_level: cuint32); inline;
begin
  asm
    eieio
	end;
  lock^.lock := 0;
  _CPU_ISR_Restore(isr_level);
end;

procedure read_lock_init(lp: prwlock_t); inline;
begin
  lp^ := RW_LOCK_UNLOCKED
end;

procedure read_lock(rw: prwlock_t); inline;
var
  tmp: cuint32;
begin
  asm
    b   .L2
  .L1:
    lwzx            r3,0,r4
    cmpwi           0,r3,0
    blt+            .L1
  .L2:
    lwarx           r3,0,r4
    addic.          r3,r3,1
    ble-            .L1
    stwcx.          r3,0,r4
    bne-            .L2
    isync
  end;
end;

procedure read_unlock(rw: prwlock_t); inline;
var
  tmp: cuint32;
begin
  asm
    eieio
  .L1:
    lwarx           r3,0,r4
    addic           r3,r3,-1
    stwcx.          r3,0,r4
    bne-            .L1
  end;
end;

procedure write_lock(rw: prwlock_t); inline;
var
  tmp: cuint32;
begin
  asm
    b              .L2
  .L1:
    lwzx            r3,0,r4
    cmpwi           0,r3,0
    bne+            .L1
  .L2:
    lwarx           r3,0,r4
    cmpwi           0,r3,0
    bne-            .L1
    stwcx.          r5,0,r4
    bne-            .L2
    isync
  end;
end;

procedure write_unlock(rw: prwlock_t); inline;
begin
  asm
    eieio
  end;
  rw^.lock := 0;
end;
{$ENDIF}

