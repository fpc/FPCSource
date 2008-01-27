#include <stdio.h>
#include <sys/soundcard.h>

int main ()
{
  printf("SNDCTL_DSP_STEREO = %d;\n",SNDCTL_DSP_STEREO);
  printf("SNDCTL_DSP_RESET = %d;\n",SNDCTL_DSP_RESET);
  printf("SNDCTL_DSP_SYNC = %d;\n",SNDCTL_DSP_SYNC);
  printf("SOUND_PCM_WRITE_BITS = %d;\n",SOUND_PCM_WRITE_BITS);
  printf("SOUND_PCM_WRITE_CHANNELS = %d;\n",SOUND_PCM_WRITE_CHANNELS);
  printf("SOUND_PCM_WRITE_RATE = %d;\n",SOUND_PCM_WRITE_RATE);
  printf("{ Sizeof(short) = %d; }\n",sizeof(short));
}
