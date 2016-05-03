{$ifdef NDS_INTERFACE}

type
  FifoMessageType = integer;
const
  SOUND_PLAY_MESSAGE        : FifoMessageType = $1234;
  SOUND_PSG_MESSAGE         : FifoMessageType = $1235;
  SOUND_NOISE_MESSAGE       : FifoMessageType = $1236;
  MIC_RECORD_MESSAGE        : FifoMessageType = $1237;
  MIC_BUFFER_FULL_MESSAGE   : FifoMessageType = $1238;
  SYS_INPUT_MESSAGE         : FifoMessageType = $1239;
	SDMMC_SD_READ_SECTORS     : FifoMessageType = $123A;
	SDMMC_SD_WRITE_SECTORS    : FifoMessageType = $123B;
	SDMMC_NAND_READ_SECTORS   : FifoMessageType = $123C;
	SDMMC_NAND_WRITE_SECTORS  : FifoMessageType = $123D;

type
{$PACKRECORDS 4} 
  FifoMessage = record
    _type: cuint16;
    case integer of
      0: (
        SundPlay: record
          _type: cuint16;
          data: pointer;
          dataSize: cuint16;
          loopPoint: cuint16;
          freq: cuint16;
          volume: cuint8;
          pan: cuint8;
          loop: cbool;
          format: cuint8;
        end;
      );
      1: (
        SoundPsg: record
          freq: cuint16;
          dutyCycle: cuint8;
          volume: cuint8;
          pan: cuint8;
          channel: cuint8;
        end;
      );
      2: (
        MicRecord: record
          buffer: pointer;
          bufferLength: cuint32;
          freq: cuint16;
          format: cuint8;
        end;
      );
      3: (
        MicBufferFull: record
          buffer: pointer;
          length: cuint32;
        end;
      );
      4: (
        SystemInput: record
          touch: touchPosition;
          keys: cuint16;
        end;
      );
      5: (
        sdParams: record
          buffer: pointer;
			    startsector: cuint32;
			    numsectors: cuint32;
			  end;
		  );
      6: (
        fwParams: record
          buffer: pointer;
          address: cuint32;
          length: cuint32;
        end;      
      );  
        
  end;
  TFifoMessage = FifoMessage;
  PFifoMessage = ^FifoMessage;

{$PACKRECORDS C}   
{$endif NDS_INTERFACE}

