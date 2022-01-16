(*
Copyright 1989, 1998  The Open Group

Permission to use, copy, modify, distribute, and sell this software and its
documentation for any purpose is hereby granted without fee, provided that
the above copyright notice appear in all copies and that both that
copyright notice and this permission notice appear in supporting
documentation.

The above copyright notice and this permission notice shall be included in
all copies or substantial portions of the Software.

THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT.  IN NO EVENT SHALL THE
OPEN GROUP BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN
AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN
CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.

Except as contained in this notice, the name of The Open Group shall not be
used in advertising or otherwise to promote the sale, use or other dealings
in this Software without prior written authorization from The Open Group.
 *)

const
  MULTIBUFFER_PROTOCOL_NAME = 'Multi-Buffering';

  MULTIBUFFER_MAJOR_VERSION = 1;  { current version numbers }
  MULTIBUFFER_MINOR_VERSION = 1;  { has ClearImageBufferArea }

(*
 * update_action field
 *)
  MultibufferUpdateActionUndefined  = 0;
  MultibufferUpdateActionBackground = 1;
  MultibufferUpdateActionUntouched  = 2;
  MultibufferUpdateActionCopied     = 3;

(*
 * update_hint field
 *)
  MultibufferUpdateHintFrequent     = 0;
  MultibufferUpdateHintIntermittent = 1;
  MultibufferUpdateHintStatic       = 2;

(*
 * valuemask fields
 *)
  MultibufferWindowUpdateHint = 1 shl 0;
  MultibufferBufferEventMask  = 1 shl 0;

(*
 * mono vs. stereo and left vs. right
 *)
  MultibufferModeMono   = 0;
  MultibufferModeStereo = 1;
  MultibufferSideMono   = 0;
  MultibufferSideLeft   = 1;
  MultibufferSideRight  = 2;

(*
 * clobber state
 *)
  MultibufferUnclobbered        = 0;
  MultibufferPartiallyClobbered = 1;
  MultibufferFullyClobbered     = 2;

(*
 * event stuff
 *)
  MultibufferClobberNotifyMask = $02000000;
  MultibufferUpdateNotifyMask  = $04000000;

  MultibufferClobberNotify = 0;
  MultibufferUpdateNotify  = 1;
  MultibufferNumberEvents  = MultibufferUpdateNotify + 1;

  MultibufferBadBuffer    = 0;
  MultibufferNumberErrors = MultibufferBadBuffer + 1;
