
{$include helper.inc}
{$include ndstypes.inc}
{$include bios.inc}
{$include card.inc}
{$include debug.inc}
{$include disc_io.inc}
{$include dma.inc}
{$include interrupts.inc}
{$include ipc.inc}
{$include memory.inc}
{$include touch.inc}
{$include fifocommon.inc}
{$include system.inc}
{$include timers.inc}
{$include fifomessages.inc}

{$include linkedlist.inc}
{$include dynamicArray.inc}
 
{$ifdef ARM9}
  {$include arm9/sassert.inc}
  {$include arm9/background.inc}
  {$include arm9/cache.inc}
  {$include arm9/console.inc}
  {$include arm9/keyboard.inc}
  {$include arm9/exceptions.inc}
  {$include arm9/image.inc}
  {$include arm9/input.inc}
  {$include arm9/math.inc}
  {$include arm9/pcx.inc}
  {$include arm9/rumble.inc}
  {$include arm9/sound.inc}
  {$include arm9/trig_lut.inc}
  {$include arm9/video.inc}
  {$include arm9/videoGL.inc}
  {$include arm9/postest.inc}
  {$include arm9/boxtest.inc}
  {$include arm9/sprite.inc}
  {$include arm9/decompress.inc}
  {$include arm9/ndsmotion.inc}
{$endif ARM9}

{$ifdef ARM7}
  {$include arm7/audio.inc}
  {$include arm7/clock.inc}
  {$include arm7/input.inc}
  {$include arm7/serial.inc}
  { $include arm7/system.inc} 
  {$include arm7/touch.inc}
{$endif ARM7}
