{
  Copyright (c) 1998 Erland Van Olmen (erlandvo@hotmail.com)
  Protected Mode Ready - but HW detect will not work, only SW detect yet

  For the SB 1.0, SB PRO 1.0, SB 2.0, SB PRO 2.0 & compatibles there are some
  remarkable frequencies:

           Freq. (Hz) | Reg. value
           -----------|-----------
             8000     |   131
            10000     |   156
            12500     |   176
            15625     |   192
            20000     |   206
            25000     |   216
            31250     |   224
            40000     |   231

ўўBugfixes/changes/additions since version 1.0 (first public release):

      - Reduced clicks when using interpolation.

ўўBugfixes/changes/additions since version 1.1 (second public release):

      - Added MixRate Recalculation. With SB (up to SB PRO 2, NOT SB16),
        the MOD will always sound the same, no matter which MixRate you
        use (The SB16 uses a different way to set the mixRate so the
        problem doesn't exist with this card).
        Ah well. At least the differences will be minimized ;)
      - Added the WAV writer
      - removed all floating point calculations (all two of 'em ;) )
      - Added stereo mixing routines in the pascal version
      - The pascal version of the mixer is buggy and should definitely not
        be used in the DPMI version. The ASM version works fine.
        --==>>> this bug should be fixed.
      - Added SB Replay for the DPMI version
      - Added DMA Auto-Init mode. Made no sound quality improvement
        whatsoever during the process, still as clicky and distorted
        as ever, especially with small mixing buffers of course.
        Replay is much nicer in Dosbox than in win xp dos emu, I have not
        been able to test it on a real SB (Pro) card, however, on a win98
        machine with HW SB PRO emulation (ESS Audiodrive) sound was nice,
        no clicks.
      - global vol setting doesn't work in stereo, wrong register value?
      - HW detect procedure is oudated and not DPMI compatible. Do not use it!
}

{$I-,N+,E-,R-}
{DEFINE USE_BP_MIXER}     { ENABLE THIS IF YOU WANT TO USE THE PASCAL MIXER }
Unit SB_DRV1;

interface

Uses
  NewCrt,   { the Crt Delay fn causes runtime error 200 on > 200Mhz machines }
  MODType1,
  Replay;

CONST
  DefMBufLn = $400;                       { Last Digit MUST be 0!            }
  NoFilter = 0;
  HWFilter = 1;
  LIFilter = 2;
  CIFilter = 3;
  MxFilter = CIFilter;

  SBVolumeBoost: Boolean = False;      { If SB HW global volume is used too  }
  StereoReplay : Boolean = False;      { If SB replay is in stereo           }
  SixteenBitMix: Boolean = False;      { 16bit mixing (WAV writer only)      }
  MixRate      : Word = 44100;

  WriteWAV     : Boolean = false;         { if set to true, a WAV file is    }
  WAVFilename  : String  = 'default.wav'; {      created rather than playback}
  Saturation   : Word = 0;

VAR
  SBDevice     : TOutputDevice;           { the driver                       }


Procedure SetFilter (Filter: Byte);
Function  GetFilter: Byte;
Procedure SetAmplify(SB_Amp: Word);


{лллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллл}
{лллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллл}


implementation


Uses
{$IFDEF DPMI}
  WinAPI,
{$ENDIF}
  Dos,
  HardWare;

Type
{ Do *NOT* change order or insert fields! it'll corrupt the asm mix routines!}
  ChnMixInfoType = packed Record
{0 }OnMix       : Boolean; { If this channel is active                       }
{1 }Vol,                   { the channel volume (for mono mixing)            }
{2 }LeftVol,               { the left -side channel volume (for panning)     }
{3 }RightVol    : Byte;    { the right-side channel volume (for panning)     }
{Info about the sample: }
{4 }RepeatSample: Boolean; { If the sample is a looping one                  }
{5 }RepeatOffset,          { Repeat Offset of the sample                     }
{7 }RepeatLength,          { Repeat Length of the sample                     }
{9 }Length      ,          { Length        of the sample                     }
{11}SampleSeg   ,          { Segment of the sample data                      }
{13}SampleOfs   : Word;    { Offset  of the sample data                      }
{15}IncEr,                 { Frequency constant                              }
{19}RealIndex   : LongInt; { Index in sample data                            }
  End; { size = 23 bytes }
{
  LeftVol  = ($FF-Pan) * Scale;     // ex. for scale: 1, 0.5, 0.25, etc.
  RightVol = Pan       * Scale;
}
  MixInfoType = Array[1..32] of ChnMixInfoType;
  MixInfoPTR  = ^MixInfoType;

  _DWBuf   = array[0..16000] of LongInt;
  DBufType = ^_DWBuf;

CONST
  MixBufLen: word = DefMBufLn;        { Ideal: $1000; Min: $1000; Max: $3F00 }
  MaxSamples = 31;

{Some SB registers & register indices: --------------------------------------}
  SBPROFilterBit = $20;   { used to be $32 in MOD4.PAS... now $20???}
  SBPROStereoBit = $2;

  SBMasterVolReg = $22;
  SBVoiceVolReg  = $4;
  SBFMVolReg     = $26;
  SBOutPutReg    = $E;
  SBResetReg     = $0;

{Var's for the SB Mixing device(s): -----------------------------------------}
  Amplify          : Word = 20;        { range: 8..255, resolution: 4 bits   }
  CallBpm          : Word = 40000 div 50;
  CallBpmMod       : Word = 40000 mod 50;
  CallBpmModCnt    : LongInt = 0;
  MixCount         : Word = 0;
  MixInfo_Size     : Word = SizeOf(ChnMixInfoType);
  BuffersAllocated : Boolean = False;
  Interpolation    : Byte    = CIFilter;{ If interpolation should be done    }
  UseOutFilter     : Boolean = False;  { If the SB PRO Filter should be used }
  DMA_Auto_Init    : Boolean = false;


VAR
  MaxA, MinA   : LongInt;              { amplify values                      }

{For the WAV writing device: ------------------------------------------------}
  WAV_F            : File;

{Following var's are needed for SB I/O routines: ----------------------------}
  DSP_RESET,
  DSP_READ_DATA,
  DSP_WRITE_DATA,
  DSP_WRITE_STATUS,
  DSP_DATA_AVAIL   : Word;

{These three buffers are used by the SB double-buffering mixing routine: ----}
  TMixBuffer       : DBufType;
  PlayBuffer,
  MixBuffer
{$IFDEF DPMI}
                   : PPointer;
{$ELSE}
                   : PByteBuffer;
{$ENDIF}

{These var's are backup values of the SB Mixer Register values: -------------}
  OldMasterVolume,
  OldVoiceVolume,
  OldOutPutMode    : Byte;

  MixIRQActive     : Boolean;               { If the IRQ procedure is active }
  MixInfo          : MixInfoType;
  SamplePointers   : Array[1..MaxSamples] of PByteBuffer;
  OldSBIRQInterrupt: Procedure;    { The previously-installed SB IRQ handler }


{лллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллл}
{лллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллл}

{$IFDEF USE_BP_MIXER}
{$I mpts\bp_mixer.pas} { Include PASCAL MIXER }
{$ELSE}
{$F-}
Procedure SBUpdateMultipleStepsEffects; BEGIN UpdateMultipleStepsEffects; END;
Procedure SBUpdateNotes;                BEGIN UpdateNotes;                END;
{$L DMA_MIX.OBJ}             { Include the ASM file with the mixing routines }
Procedure _SB_Mixer;         external;
Procedure _SwapBuffers;      external;
Procedure _UpdateBPM;        external;
{$F+}
{$ENDIF}

{$IFDEF DPMI}
Procedure Playback                   (Sound: PPointer; Size: Word); Forward;
Procedure PlaybackDMAAutoInitStart   (Sound: PPointer; Size: Word); Forward;
{$ELSE}
Procedure Playback                   (Sound:  Pointer; Size: Word); Forward;
Procedure PlaybackDMAAutoInitStart   (Sound:  Pointer; Size: Word); Forward;
{$ENDIF}

procedure WriteDSP(value: byte); Forward;

{лллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллл}
{лллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллл}
{ллллллллллллллллл Here come the SoundBlaster I/O Routines: ллллллллллллллллл}
{лллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллл}
{лллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллл}


{$IFDEF DPMI}
Procedure _SWAPBUFFERS;                            { swap mix & play buffers }
VAR
  temp: PPointer;
BEGIN
  CopyPPointer(MixBuffer , temp      );
  CopyPPointer(PlayBuffer, MixBuffer );
  CopyPPointer(temp      , PlayBuffer);
{  temp:=MixBuffer; MixBuffer:=PlayBuffer; PlayBuffer:=temp;}
END;
{$ELSE}
Procedure _SWAPBUFFERS;                            { swap mix & play buffers }
VAR
  temp: PByteBuffer;
BEGIN
  temp:=MixBuffer; MixBuffer:=PlayBuffer; PlayBuffer:=temp;
END;
{$ENDIF}

{****************************************************************************}

{$F+}
procedure IRQProc; interrupt;
Var
   temp: byte;
BEGIN
  Asm cli End;
  If Not (MixIRQActive OR ModInfo.FinishedReplay) then
    Begin
      MixIRQActive:=True;
      temp:=port[DSP_DATA_AVAIL];{ Aknowledge IRQ to SB }

      _SwapBuffers;
      If Not DMA_Auto_Init then Playback(PlayBuffer, MixBufLen);

      _SB_Mixer;
      MixIRQActive:=False;                     { Indicate we're ready mixing }
    End
  Else temp:=port[DSP_DATA_AVAIL];             { Aknowledge IRQ to SB }

  If SBDevice.MIRQ>7 then port[$A0]:=$20; { IRQ>7 ? > send EOI to slave  PIC }
  port[$20]:=$20;                         { send EOI to master PIC }
  asm sti end;
END;

{лллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллл}

Procedure SetFilter(Filter: Byte);
Begin
  Case Filter of
    HWFilter: begin UseOutFilter:=true ; Interpolation:=NoFilter; end;
    LIFilter: begin UseOutFilter:=False; Interpolation:=LIFilter; end;
    CIFilter: begin UseOutFilter:=False; Interpolation:=CIFilter; end;
   else; begin UseOutFilter:=False; Interpolation:=0; end;
  end;
End;

{лллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллл}

Function  GetFilter: Byte;
Begin
  GetFilter:=Interpolation;
End;

{лллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллл}

Procedure SetAmplify(SB_Amp: Word);
Begin
  If SB_Amp>255 then SB_Amp:=255;
  If SB_Amp<8   then SB_Amp:=8;
  SB_Amp:=SB_Amp div ModInfo.Nrchannels;
  Amplify:=SB_Amp;
End;

{лллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллл}

Function ResetDSP: boolean;
BEGIN
{ Calculate the port addresses: ---------------------------------------------}
  DSP_RESET       :=SBDevice.MPort+$6;
  DSP_READ_DATA   :=SBDevice.MPort+$A;
  DSP_WRITE_DATA  :=SBDevice.MPort+$C;
  DSP_WRITE_STATUS:=SBDevice.MPort+$C;
  DSP_DATA_AVAIL  :=SBDevice.MPort+$E;

{ Reset the DSP, and give some nice long delays just to be safe: ------------}
  Port[DSP_RESET]:=1; Delay(10); Port[DSP_RESET]:=0; Delay(10);
  If ((Port[DSP_DATA_AVAIL] And $80)=$80) And (Port[DSP_READ_DATA]=$AA)
    then ResetDSP:=True
  Else   ResetDSP:=False;
END;

{лллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллл}

procedure WriteDSP(value: byte);
VAR I: LongInt;
begin
  I:=0;
  while (Port[DSP_WRITE_STATUS] And $80 <> 0) and (I<1000000000) do Inc(I);
  If(I<1000000000) then Port[DSP_WRITE_DATA]:=value;
end;

{лллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллл}

function ReadDSP: byte;
VAR I: LongInt;
begin
  I:=0;
  while ((Port[DSP_DATA_AVAIL] And $80) = 0) and (I<1000000000) do Inc(I);
  If(I<1000000000) then ReadDSP:=Port[DSP_READ_DATA];
end;

{лллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллл

procedure WriteDAC(level: byte);
begin
  WriteDSP($10);
  WriteDSP(level);
end;

{лллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллл

function ReadDAC: byte;
begin
  WriteDSP($20);
  ReadDAC := ReadDSP;
end;

{лллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллл}

function SpeakerOn: byte;
begin
  WriteDSP($D1);
end;

{лллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллл}

Function SpeakerOff: byte;
begin
  WriteDSP($D3);
end;

{лллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллл}

Procedure SetMixerReg(index, value: byte);
begin
  Port[SBDevice.MPort+4]:=index;
  Port[SBDevice.MPort+5]:=value;
end;

{лллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллл}

Function GetMixerReg(index: byte): byte;
begin
  Port[SBDevice.MPort+4]:=index;
  GetMixerReg:=Port[SBDevice.MPort+5];
end;

{лллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллл}

{$IFDEF DPMI}
Procedure Playback(Sound: PPointer; Size: Word);
{$ELSE}
Procedure Playback(Sound:  Pointer; Size: Word);
{$ENDIF}
VAR
  page, offset: Word;
  _MixRate    : LongInt;
{$IFDEF DPMI}
  LinAddr     : LongInt;
{$ENDIF}
Begin
{ Set up the DMA chip: ------------------------------------------------------}
{$IFDEF DPMI}
  LinAddr:=LongInt(LongRec(LongInt(Sound.rm)).hi) shl 4 +
                   LongRec(LongInt(Sound.rm)).lo;
  page   :=LinAddr shr 16;
  offset :=LinAddr and $FFFF;
{$ELSE}
  page  :=(Seg(Sound^) + Ofs(Sound^) shr 4) shr 12;
  offset:= Seg(Sound^) Shl 4 + Ofs(Sound^);
{$ENDIF}
  Port[$0A]:=4+SBDevice.MDMA; { set mask (disable channel) + select dma chn  }
  Port[$0B]:=$48+SBDevice.MDMA; { read op., sig. mode single cyc. dma chn }
  Port[$0C]:=0;               { clear flipflop (reset the internal pointers) }
  Port[$02]:=Lo(offset);
  Port[$02]:=Hi(offset);
  Port[$83]:=page;        { hardcoded for DMA channel 1 :s }
  Port[$03]:=Lo(size-1);  { should be size-1 but one more byte reduces clics }
  Port[$03]:=Hi(size-1);  { should be size-1 but one more byte reduces clics }
  Port[$0A]:=SBDevice.MDMA;               { select DMA channel               }

  WriteDSP($48);          { set mode to 8 bit PCM data }
  WriteDSP(Lo(size-1));
  WriteDSP(Hi(size-1));

{ Set the playback type (8-bit): --------------------------------------------}
  If StereoReplay then _MixRate:=LongInt(MixRate)*2
  Else                 _MixRate:=MixRate;

{ Go Baby Go! ---------------------------------------------------------------}
  If _MixRate<22050 then WriteDSP($14)   { normal mode     }
  Else                   WriteDSP($91);  { high speed mode }
End;

{лллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллл}

{$IFDEF DPMI}
procedure PlaybackDMAAutoInitStart(Sound: PPointer; Size: Word);
{$ELSE}
procedure PlaybackDMAAutoInitStart(Sound: Pointer; Size: Word);
{$ENDIF}
VAR
  page, offset: Word;
  _MixRate: LongInt;
{$IFDEF DPMI}
  LinAddr     : LongInt;
{$ENDIF}
Begin
{$IFDEF DPMI}
  LinAddr:=LongInt(LongRec(LongInt(Sound.rm)).hi) shl 4 +
                   LongRec(LongInt(Sound.rm)).lo;
  page   :=LinAddr shr 16;
  offset :=LinAddr and $FFFF;
{$ELSE}
  offset:= Seg(sound^) Shl 4 + Ofs(sound^);
  page  :=(Seg(sound^) + Ofs(sound^) shr 4) shr 12;
{$ENDIF}

{ Set up the DMA chip: ------------------------------------------------------}
  Port[$0A]:=4+SBDevice.MDMA; { set mask (disable channel) + select dma chn  }
  Port[$0B]:=$58+SBDevice.MDMA; { read op., signal mode auto init, dma chn   }
  Port[$0C]:=0;               { clear flipflop (reset the internal pointers) }
  Port[$02]:=Lo(offset);
  Port[$02]:=Hi(offset);
  Port[$83]:=page;            { hardcoded for DMA channel 1 :s }
  Port[$03]:=Lo(size-1);
  Port[$03]:=Hi(size-1);
  Port[$0E]:=0;               { is unnecessary I believe }
  Port[$0A]:=SBDevice.MDMA;   { unset mask (enable channel) + select dma chn }

  Size:=Size div 2;           { Generate IRQ when half of buffer is played }
  WriteDSP($48);              { set mode to 8 bit PCM data   }
  WriteDSP(Lo(size-1));       { size-1 is definitely needed! }
  WriteDSP(Hi(size-1));       { size-1 is definitely needed! }

{ Set the playback type (8-bit): --------------------------------------------}
  If StereoReplay then _MixRate:=LongInt(MixRate)*2
  Else                 _MixRate:=MixRate;
{ Go Baby Go! ---------------------------------------------------------------}
  If _MixRate<22050 then WriteDSP($1C)    { normal mode     }
  Else                   WriteDSP($90);   { high speed mode }
End;

{лллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллл}
{лллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллл}
{лллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллл}
{лллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллл}

Function OpenCard(NrVoices: Byte): Word;
Var
  SBReg, tmp: Byte;
  I    : Word;
  AllocOK: Boolean;
  rm_seg, rm_ofs, LinAddr: LongInt;
BEGIN
  GetIntVec($8+SBDevice.MIRQ, @OldSBIRQInterrupt);
  IF (SBDevice.Subtype<$300) AND (NOT WriteWAV) then StereoReplay:=False;
  IF SBDevice.Subtype<$201 then DMA_auto_Init:=false
  Else                          DMA_auto_Init:=true;
{  DMA_auto_Init:=false; { TEMP DEBUG !!! ***** }

{Init Amplify barrier values: -----------------------------------------------}
{  MinA:=LongInt(-128*64*16*NrVoices);
  MaxA:=LongInt( 127*64*16*NrVoices);}
  MinA:=LongInt(-128*64*16);
  MaxA:=LongInt( 127*64*16);
  tmp:=ModInfo.NrChannels; ModInfo.NrChannels:=NrVoices;
  SetAmplify(Amplify);
  ModInfo.NrChannels:=tmp;

{  Case Filter of
    SBPROHW      : UseOutFilter :=True;
    Interpolative: Interpolation:=True;
  End;                                 }

  If Not WriteWAV then
  begin
{Is there a "DSP" chip? -----------------------------------------------------}
    If Not ResetDSP then Begin OpenCard:=255; exit; end; { Reset DSP         }
    SpeakerOn;                                           { Connect Amplifier }
    SetMixerReg(SBResetReg, 0);                          { Reset Mixer       }
{Save the old register values: ----------------------------------------------}
    OldOutPutMode  :=GetMixerReg(SBOutPutReg);
    OldVoiceVolume :=GetMixerReg(SBVoiceVolReg);
    OldMasterVolume:=GetMixerReg(SBMasterVolReg);
{Set the SB output mode: Low/High Filter, Stereo/Mono. ----------------------}
    SBReg:=0;
    If StereoReplay then
      If UseOutFilter then SBReg:=SBPROStereoBit AND ($FF XOR SBPROFilterBit)
      Else                 SBReg:=SBPROStereoBit OR  SBPROFilterBit
    Else
      If UseOutFilter then SBReg:=0
      Else                 SBReg:=SBPROFilterBit;
    SetMixerReg(SBOutPutReg, SBReg);
{Set the SB output volume: --------------------------------------------------}
    GlobalVolume:=(Word(GlobalVolume-10) * $10) div 100;
{ globalvolume setting -> disabled becoz it causes balances to be modified!!!
    SetMixerReg(SBVoiceVolReg, GlobalVolume shl 4 + GlobalVolume);}
    If SBVolumeBoost then SetMixerReg(SBMasterVolReg, $FF);{}
  end;

  If not BuffersAllocated Then
  Begin
{Allocate mem for the DWORD temporary MIX Buffer: ---------------------------}
    If MaxAvail>(MixBufLen shl 2) then GetMem(TMixBuffer, MixBufLen shl 2)
    Else Begin OpenCard:=255; exit; end;
{be sure to get 2 buffers that doesn't cross page breaks: -------------------}
{ (Stupid DMA controller can't transfer blocks that cross page breaks) }
{$IFNDEF DPMI}
    If Not GetDMABuffer(Pointer(MixBuffer), MixBufLen*2+32) Then
                                               Begin OpenCard:=255; exit; end;
    If (ofs(MixBuffer^) and $F) <> 0 then { make buffers 16 byte aligned }
      Begin
{        Inc(LongInt(MixBuffer), $10000);}
        Inc(LongInt(MixBuffer), $10);
        MixBuffer:=Pointer(LongInt(MixBuffer) and $FFFFFFF0);
      End;
    PlayBuffer:=Ptr(Seg(MixBuffer^)+MixBufLen shr 4, 0);
    For I:=0 to MixBufLen*2-1 do MixBuffer^[I]:=0;
{$ELSE}
    If Not GetDMABuffer(        MixBuffer , MixBufLen*2+32) then
                                               Begin OpenCard:=255; exit; end;
{        GlobalFix(LongREC(LongInt(MixBuffer.pm)).lo); {should be unnecessary}
    PlayBuffer.rm:=Pointer(LongInt(LongRec(MixBuffer.rm).hi +
                                                   (MixBufLen shr 4)) shl 16);
    GetMappedDPMIPtr(PlayBuffer.pm, PlayBuffer.rm, MixBufLen+32);
    ClearDOS(MixBuffer, MixBufLen*2+32);
{$ENDIF}
    BuffersAllocated:=True;
  End;

{ Set the playback frequency: -----------------------------------------------}
  If Not WriteWAV then
    begin
      If StereoReplay then I:=256 - 1000000 div (LongInt(MixRate)*2)
      Else                 I:=256 - 1000000 div          MixRate;
      WriteDSP($40);
      WriteDSP(I);
      If StereoReplay then MixRate:=1000000 div (2*(256 - I))
      Else                 MixRate:=1000000 div    (256 - I);
    end
  Else    { for maximum compatibility with the legacy WAV PCM file format }
    Begin
      If MixRate>32000 then MixRate:=44100
      Else If MixRate>16000 then MixRate:=22050
           Else                  MixRate:=11025;
    End;
  OpenCard:=1;
END;
{лллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллл}

Function IsDec(Test: Char): Boolean;
VAR
   T: Byte;
BEGIN
  T:=Ord(Test); If (T>=48) AND (T<=57) then IsDec:=True Else IsDec:=False;
END;

{лллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллл}

Function IsHex(Test: Char): Boolean;
VAR
   T: Byte;
BEGIN
  T:=Ord(Test);
  If ((T>=48) AND (T<=57 )) OR ((T>=65) AND (T<=70 )) OR
     ((T>=97) AND (T<=102)) then IsHex:=True Else IsHex:=False;
END;

{лллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллл}

Function CharToHex(Test: Char): Byte;
VAR
  T: Byte;
BEGIN
  T:=Ord(Test);
  If ((T>=48) AND (T<=57 )) then CharToHex:=T-48 Else
  If ((T>=65) AND (T<=70 )) then CharToHex:=T-55 Else
  If ((T>=97) AND (T<=102)) then CharToHex:=T-87 Else T:=255; {not hex}
END;

{лллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллл}

Procedure IRQ_Test; Interrupt;
VAR
  temp: Byte;
BEGIN
  Asm cli End;
  temp:=port[DSP_DATA_AVAIL];
  MixIRQActive:=True;
  If SBDevice.MIRQ>7 then port[$A0]:=$20;  { IRQ > 7 ? -=> send EOI to slave PIC }
  Port[$20]:=$20;
  Asm sti End;
END;

{============================================================================}

Function HW_Check: Boolean;     { checks if the detected hardware is present }
Const
  BufLen = 100;
VAR
  Result: Boolean;
{$IFDEF DPMI}
  tmpBuf: PPointer;
{$ELSE}
  tmpBuf: array[0..BufLen-1] of Byte;
{$ENDIF}
  I: Byte;
  temp: Word;
BEGIN
  HW_Check:=False;

{ Check the BasePort: -------------------------------------------------------}
  Result:=ResetDSP; If not Result then Exit;

  WriteDSP($E1); Temp:=Word(ReadDSP) SHL 8; Inc(Temp, ReadDSP);
  SBDevice.SubType:=Temp;
  If SBDevice.SubType<$200 then SBDevice.ID:=SB100Device Else
  If SBDevice.SubType<$201 then SBDevice.ID:=SB200Device Else
  If SBDevice.SubType<$300 then SBDevice.ID:=SB201Device Else
  If SBDevice.SubType<$400 then SBDevice.ID:=SBPRODevice Else
  SBDevice.ID:=SB16Device; {or newer}
{  If SBDevice.SubType>Temp then exit;     { SB IS present, but an older one }

{ Check the IRQ/DMA: --------------------------------------------------------}
  GetIntVec($8+SBDevice.MIRQ, Addr(OldSBIRQInterrupt)); {Save old IRQ handler}
  SetIntVec($8+SBDevice.MIRQ, Addr(IRQ_Test));
{$IFDEF DPMI}
{$ELSE}
  For I:=0 to BufLen-1 do tmpBuf[I]:=0;
{$ENDIF}
  irq_Enable(SBDevice.MIRQ);
  MixIRQActive:=False;

{ Set the playback frequency & start transfer: ------------------------------}
  WriteDSP($40); WriteDSP(256 - 1000000 div 20000);

{  Playback(@tmpBuf, BufLen); Delay(50);         { Give a nice long delay }
  HW_Check:=MixIRQActive; MixIRQActive:=False;      { Did I got the IRQ?     }
  irq_Disable(SBDevice.MIRQ);
  setIntVec($8+SBDevice.MIRQ, Addr(OldSBIRQInterrupt)); { Restore old handler}
END;

{лллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллл}

Function SBSW_Detect: Boolean;          { Detects All SB-cards via software. }
{ SoundBlaster's Environment Variable format:

  BLASTER=Ax Ip Dq Hr Ms Pt Ty Ez

  x: Primary I/O port. (in hex.)
  p: IRQ channel       (in hex.)
  q: Primary (8-bit) DMA channel
  r: Secondary (16-bit) DMA channel (SB16+ only)
  s: Primary I/O Port of the SB-Mixer (in hex.)
  t: MIDI I/O Port                    (in hex.)
  y: Type of SB card. Possible values:
        1: SoundBlaster 1.x
        2: SoundBlaster PRO 1 (old SB PRO)
        3: SoundBlaster 2.0
        4: SoundBlaster PRO 2, PRO 3 ("new" SB PRO)
        5: SoundBlaster PRO MCV (MicroChannel BUS), same as SB PRO 2
        6: SoundBlaster 16 family.
  z: the 'E' parameter is ONLY needed for SB AWE32 cards. Its default value
     is 620 ("E620").

Ex: BLASTER=A220 I7 D1 T4 (SB PRO 2 at 220h, using DMA channel 1 & IRQ 7) }

VAR
  tmp, EVS_index: Byte;
  EVS_Field: Char;
  EVS: String;            {Environment string}
  _Base, _Base2, _IRQ1, _IRQ2, _DMA1, _DMA2, I, _Type: Word;
  HWOk, EnvOk: Boolean;
Label TestHW;
BEGIN
  EVS_index:=1; EVS:=GetEnv(SBDevice.MEnvStr);

  If EVS<>'' then
    begin
      EnvOk:=True; SBDevice.SWDetected:=True; {Temporary!!!}
      Repeat
        EVS_Field:=Upcase(EVS[EVS_index]);
        Case EVS_Field of
          'A': Begin
                 If IsDec(EVS[EVS_index+2]) then
                 Begin
                   _Base:=$200+Word(CharToHex(EVS[EVS_index+2]))*$10;
                   SBDevice.MPort:=_Base;
                 End;
                 Inc(EVS_Index, 5);
               End;
          'I': Begin
                 tmp:=Ord(EVS[EVS_index+1]);
                 If (tmp<=Ord('9')) AND (tmp>=Ord('0')) then _IRQ1:=tmp-48
                 Else If (tmp>=Ord('A')) AND (tmp<=Ord('F')) then
                                 _IRQ1:=tmp-55
                      Else If (tmp>=Ord('a')) AND (tmp<=Ord('f')) then
                                 _IRQ1:=tmp-87;
                 SBDevice.MIRQ:=_IRQ1;
                 Inc(EVS_Index, 3);
               End;
          'D': Begin
                 tmp:=Ord(EVS[EVS_index+1]);
                 If (tmp<=Ord('3')) AND (tmp>=Ord('0')) then
                   Begin _DMA1:=tmp-48; SBDevice.MDMA:=_DMA1; End;
                 Inc(EVS_Index, 3);
               End;
          'H': Begin
                 tmp:=Ord(EVS[EVS_index+1]);
                 If (tmp<=Ord('7')) AND (tmp>=Ord('3')) then
                   Begin _DMA2:=tmp-48; SBDevice.SDMA:=_DMA2; End;
                 Inc(EVS_Index, 3);  {are 8-bit DMA channels allowed here?}
               End;
{          'M': Begin // MixerPort, useless
               End;}
          'P': Begin
                 _Base2:=0;
                 If IsHex(EVS[EVS_index+1]) then Inc(_Base2, $100*CharToHex(EVS[EVS_index+1])) Else EnvOk:=False;
                 If IsHex(EVS[EVS_index+2]) then Inc(_Base2,  $10*CharToHex(EVS[EVS_index+2])) Else EnvOk:=False;
                 If IsHex(EVS[EVS_index+3]) then Inc(_Base2,      CharToHex(EVS[EVS_index+3])) Else EnvOk:=False;
                 If EnvOk then SBDevice.SPort:=_Base2;
                 Inc(EVS_Index, 5);
               End;
          'T': Begin
                 _Type:=CharToHex(EVS[EVS_index+1]);
                 Inc(EVS_Index, 3);
               End;
          Else; Begin {unknown, skip}
                  While ((EVS_index<Length(EVS)) AND (EVS[EVS_index]<>' ')) Do Inc(EVS_index);
                  If (EVS[EVS_index]=' ') AND (EVS_index<Length(EVS)) then Inc(EVS_index);
                End;
        End;
      Until EVS_index>=Length(EVS);

      With SBDevice do
      Case _Type of
        1:  Begin SubType:=$100; ID:=SB100Device; End; { SB 1.0              }
        2:  Begin SubType:=$200; ID:=SB200Device; End; { SB PRO 1 (old)      }
        3:  Begin SubType:=$201; ID:=SB201Device; End; { SB 2.0              }
        4:  Begin SubType:=$300; ID:=SBPRODevice; End; { SB PRO 2 or PRO 3   }
        5:  Begin SubType:=$300; ID:=SBPRODevice; End; { Same, with MCV BUS  }
        6:  Begin SubType:=$400; ID:=SB16Device;  End; { SB 16 (ASP)         }
      Else; Begin SubType:=$100; ID:=SB100Device; End; { ? -> SB 1.0 Assumed }
      End;
    end
  Else Begin SBDevice.SWDetected:=False; EnvOk:=False; End;

  If SBDevice.ID=SBPRODevice then
  Begin
    If MixRate<=22050 then StereoReplay:=True;
  End;

{  SBSW_Detect:=HW_Check;}
  SBSW_Detect:=True;
END;

{лллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллл}
{ Warning: this function may hang the system! }
Function HW_Detect: Boolean;     { Detects SB-compatibel cards via hardware. }
Const
  BufLen = 100;
VAR
  Found, Result: Boolean;
  tmpBuf: array[0..BufLen-1] of Byte;
  I: Word;

{============================================================================}

Function CheckIRQ(IRQNr: Byte): Boolean;
VAR
   Freq: Word;
BEGIN
  SBDevice.MIRQ:=IRQNr; MixIRQActive:=False;
  setIntVec($8+SBDevice.MIRQ, Addr(IRQ_Test));

{ Set the playback frequency & start transfer: ------------------------------}
  WriteDSP($40); WriteDSP(256 - 1000000 div I);
  irq_Enable(SBDevice.MIRQ);
{  Playback(@tmpBuf, BufLen); Delay(50);            { Give a nice long delay }
  CheckIRQ:=MixIRQActive; MixIRQActive:=False;      { Did I got the IRQ?     }
  irq_Disable(SBDevice.MIRQ);
END;

{============================================================================}
Label ExitOK;
BEGIN
  HW_Detect:=False;  SBDevice.HWDetected:=False;

{ Look after the BasePort: --------------------------------------------------}
  I:=0; Found:=False;
  Repeat
    Inc(I); SBDevice.MPort:=$200+I*$10; Found:=ResetDSP;
  Until (I>=8) or Found;
  If not Found then Exit;

{Look after the Version of the card: ----------------------------------------}
  WriteDSP($E1); I:=Word(ReadDSP) SHL 8; Inc(I, ReadDSP); SBDevice.SubType:=I;

  If SBDevice.SubType<$200 then SBDevice.ID:=SB100Device Else
  If SBDevice.SubType<$201 then SBDevice.ID:=SB200Device Else
  If SBDevice.SubType<$300 then SBDevice.ID:=SB201Device Else
  If SBDevice.SubType<$400 then SBDevice.ID:=SBPRODevice Else
  SBDevice.ID:=SB16Device; {or better}

  If SBDevice.ID=SBPRODevice then
  Begin
    If MixRate<=22050 then StereoReplay:=True;
  End;

{Check the IRQ/DMA: ---------------------------------------------------------}
  GetIntVec($8+SBDevice.MIRQ, Addr(OldSBIRQInterrupt));{ Save old IRQ handler}
  For I:=0 to BufLen-1 do tmpBuf[I]:=0;       { Make empty (= silent) buffer }
  If SBDevice.SubType>$201 then I:=45454 Else I:=22000;      { set frequency }

  HW_Detect:=True; SBDevice.HWDetected:=True;
  Found:=CheckIRQ(7); If Found then goto ExitOK;          { IRQ = IRQ nr 7 ? }
  Found:=CheckIRQ(5); If Found then goto ExitOK;          { IRQ = IRQ nr 5 ? }
  Found:=CheckIRQ(3); If Found then goto ExitOK;          { IRQ = IRQ nr 3 ? }
  Found:=CheckIRQ(2); If Found then goto ExitOK;          { IRQ = IRQ nr 2 ? }

  HW_Detect:=False; SBDevice.HWDetected:=False;
ExitOK:
  setIntVec($8+SBDevice.MIRQ, Addr(OldSBIRQInterrupt)); { Restore old handler}
END;

{лллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллл}

Function LoadMODSample(var F: File; SampleNr: Byte): Word;
VAR
  Size,
  Size2,
  Alloc : LongInt;
  K,
  Result: Word;
  SmpSeg, SmpOfs: Word;
  smpPTR: PByteBuffer;
BEGIN
  LoadMODSample:=255;
  Size:=ModInfo.Samples[SampleNr].Length; Size2:=Size;
  If Size2>MaxAlloc then Size2:=MaxAlloc;
  Alloc:=MaxAvail;
  If Alloc<(Size2+4) then Begin LoadMODSample:=OutOfMemory; Exit; End;
  GetMem(SamplePointers[SampleNr], Size2+4);

  SmpSeg:=Seg(SamplePointers[SampleNr]^);
  SmpOfs:=Ofs(SamplePointers[SampleNr]^);
  If SmpOfs<>$FFFF then Inc(SmpOfs) Else begin Inc(SmpSeg); SmpOfs:=0; end;
  smpPTR:=ptr(SmpSeg, SmpOfs);
  BlockRead(F, smpPTR^, Size2, Result);
{Fix mainly for Cubic Interpolative mixing routines: ------------------------}
  SamplePointers[SampleNr]^[0]:=Byte(-ShortInt(SamplePointers[SampleNr]^[1]));

  If Result<>Size2 then
    Begin
{Correct Sample header info: ------------------------------------------------}
      ModInfo.Samples[SampleNr].Length:=Result;
      LoadMODSample:=OutOfSampleData; Exit;
    End;

{Fix mainly for Interpolative mixing routines: ------------------------------}
  If ModInfo.Samples[SampleNr].RepeatSample then
    Begin
      SamplePointers[SampleNr]^[Size2  ]:=SamplePointers[SampleNr]^
                                   [ModInfo.Samples[SampleNr].RepeatOffset];
      SamplePointers[SampleNr]^[Size2+1]:=SamplePointers[SampleNr]^
                                   [ModInfo.Samples[SampleNr].RepeatOffset+1];
    End
  Else
    begin
      SamplePointers[SampleNr]^[Size2  ]:=SamplePointers[SampleNr]^[Size2-1];
      SamplePointers[SampleNr]^[Size2+1]:=SamplePointers[SampleNr]^[Size2-2];
    end;

  If Size>Size2 then
    Begin
      Seek(F, FilePos(F)+Size-Size2);
      ModInfo.Samples[SampleNr].Length:=Size2;
      If ModInfo.Samples[SampleNr].RepeatOffset >=
                                    ModInfo.Samples[SampleNr].Length then
        ModInfo.Samples[SampleNr].RepeatSample:=False
      Else
        ModInfo.Samples[SampleNr].RepeatLength:=
                                     ModInfo.Samples[SampleNr].Length-
                                     ModInfo.Samples[SampleNr].RepeatOffset;
    End;
  LoadMODSample:=NoError;
END;

{лллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллл}

Function SW_Detect: Boolean;
BEGIN
  With SBDevice do
  Begin
    SubType:=$100;                 { Dsp Version 1.00: SoundBlaster 1.0      }
    ID     :=MODType1.SB100Device; { SoundBlaster 1.00 reference nr          }
    Name   :=DeviceNames[ID];
    MEnvStr:='BLASTER';
    SEnvStr:='SOUND';
    MPort  :=$220;                 { Default for Main I/O Port               }
    SPort  :=$330;                 { Default for Secondary I/O Port          }
    MIRQ   :=7;                    { Default for Main IRQ                    }
    SIRQ   :=$FF;                  { Default for Secondary IRQ: unavailable  }
    MDMA   :=1;                    { Default for Main DMA channel            }
    SDMA   :=5;                    { Default for Secondary(16bit)DMA channel }
    HWDetected:=False;             { Card is not detected by Default         }
    SWDetected:=SBSW_Detect;
    SW_Detect:=SWDetected;
  End;
END;

{лллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллл}

Function CloseCard: Word;
BEGIN
  If WriteWAV then Begin CloseCard:=NoError; exit; End;
  irq_Disable(SBDevice.MIRQ);         { Disable the IRQ                      }
  SetIntVec($8+SBDevice.MIRQ, @OldSBIRQInterrupt);       { restore interrupt }
  ResetDSP;                           { Reset the DSP                        }
  SetMixerReg(SBResetReg, 0);         { Reset the Mixer Chip                 }

{Restore the old register values: -------------------------------------------}
  SetMixerReg(SBOutPutReg   , OldOutPutMode);
  SetMixerReg(SBVoiceVolReg , OldVoiceVolume);
  SetMixerReg(SBMasterVolReg, OldMasterVolume);
{  SpeakerOff;                         { Disconnect speaker (less parasites)  }
  CloseCard:=NoError;
END;

{лллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллл}

Function StartReplay: Word;
Const
  PCM_Data = $1;
  loop_gfx: array[0..3] of char = ('|', '/', '-', '\');
Var
  WaveHeader: packed record
    RIFF_ID       : array[0..3] of Char;{ 00 'RIFF' }
    chunkSize     : Longint;            { 04 filesize - 8 }
    WAVE_ID       : array[0..3] of Char;{ 08 'WAVE' }
    fmt_ID        : array[0..3] of Char;{ 12 'fmt ' }
    TagSize       : Longint;            { 16 should be 0x10 }
    Compression_ID: word;               { 20 should be 0x01 }
    NrChannels    : word;               { 22 1 = mono, 2 = stereo }
    SampleRate    : Longint;            { 24 in Hz }
    BytesPerSec   : Longint;            { 28 SampleRate*Stereo*BitRate/8 bytes }
    BytesPerSample: word;               { 32 1 = 8bit mono, 4 = 16bit stereo, ...}
    BitsPerSample : word;               { 34 8 = 8bit, 16 = 16bit }
    data_ID       : array[0..3] of Char;{ 36 'data' }
    DataLength    : Longint;            { 40 lentgh in Bytes }
  end;
  i, i2: Longint;
  h, m, s, s100, starttime, endtime, difftime: LongInt;

BEGIN
  ResetSong;
  SBDevice.SetBPM;
  If Not WriteWAV then
    Begin
      SetIntVec($8+SBDevice.MIRQ, Addr(IRQProc));
      _SB_Mixer;
      irq_Enable(SBDevice.MIRQ);
      If DMA_Auto_Init then      { Mix 2 buffers and THEN start IRQ/DMA loop }
        Begin
          _SwapBuffers;
          _SB_Mixer;
          PlaybackDMAAutoInitStart(PlayBuffer, MixBufLen*2); {Playbuffer = 1st}
        end
      Else Playback(PlayBuffer, MixBufLen);
      Playing:=True;
    End
  Else
    Begin
      With WaveHeader do
      begin
        RIFF_ID       :='RIFF';
        WAVE_ID       :='WAVE';
        fmt_ID        :='fmt ';
        TagSize       :=$10;
        Compression_ID:=PCM_Data;
        If StereoReplay then NrChannels:=2
        Else                 NrChannels:=1;
        SampleRate    :=MixRate;
        BitsPerSample :=(Ord(SixteenBitMix)+1) * 8;
        BytesPerSample:=NrChannels*(BitsPerSample div 8);
        BytesPerSec   :=LongInt(MixRate)*BytesPerSample; {typecast is needed!}
        data_ID       :='data';
      end;

      Assign(WAV_F, WAVFilename);
      I2:=IOResult; If I2<>0 then begin WriteLn('I/O error = '+I2DStr(I2)); Readkey; end;
      Rewrite(WAV_F, 1);
      I2:=IOResult; If I2<>0 then begin WriteLn('I/O error = '+I2DStr(I2)); Readkey; end;
      BlockWrite(WAV_F, WaveHeader, SizeOf(WaveHeader));
      I2:=IOResult; If I2<>0 then begin WriteLn('I/O error = '+I2DStr(I2)); Readkey; end;
      i:=0;
      Playing:=True;
      GetTime(word(h), word(m), word(s), word(s100));
      starttime:=h*360000+m*6000+s*100+s100;
      _UPDATEBPM;
      Repeat
        _SB_Mixer;
{$IFDEF DPMI}
        BlockWrite(WAV_F, MixBuffer.pm^, MixBufLen);
{$ELSE}
        BlockWrite(WAV_F, MixBuffer^   , MixBufLen);
{$ENDIF}
        inc(i);
        GotoXY(2, 1); Print(loop_gfx[i mod 4]+' '+I2DStr(i)+' buffers of '+I2DStr(MixBufLen)+' bytes written.');
      Until ModInfo.FinishedReplay;

      With WaveHeader do
      begin
        DataLength    :=i*MixBufLen;                     { length in Bytes }
        chunkSize     :=DataLength+SizeOf(WaveHeader)-8; { filesize - 8 }
      end;

      Seek(WAV_F, 0);      { update header & close file }
      I2:=IOResult; If I2<>0 then begin PrintLn('I/O error = '+I2DStr(I2)); Readkey; end;
      BlockWrite(WAV_F, WaveHeader, SizeOf(WaveHeader));
      I2:=IOResult; If I2<>0 then begin PrintLn('I/O error = '+I2DStr(I2)); Readkey; end;
      Close(WAV_F);
      I2:=IOResult; If I2<>0 then begin PrintLn('I/O error = '+I2DStr(I2)); Readkey; end;
      GetTime(word(h), word(m), word(s), word(s100));
      endtime:=h*360000+m*6000+s*100+s100;
      difftime:=endtime-starttime;
      s100:=difftime mod 100; difftime:=difftime div 100;
      s   :=difftime mod 60 ; difftime:=difftime div 60;
      m   :=difftime mod 60 ; difftime:=difftime div 60;
      h   :=difftime mod 60 ;
      Println('');
      Println('finished writing '+WAVFileName+'...');
      PrintLn('Written '+I2DStr(i)+' buffers * '+I2DStr(MixBufLen)+' = '+I2DStr(WaveHeader.Datalength div 1024)+' Kb');
      PrintLn('Written in '+I2DStr(H)+' hours, '+I2DStr(M)+' minutes, '
               +I2DStr(S)+' seconds and '+I2DStr(S100)+ ' hundreds/s.');
      Println('Hit any key to exit.');
      Readkey;
    End;
  StartReplay:=NoError;
END;

{лллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллл}

Function StopReplay: Word;
BEGIN
  irq_Disable(SBDevice.MIRQ);
{  SetIntVec($8+SBDevice.MIRQ, @OldSBIRQInterrupt);}
  StopReplay:=NoError;
END;

{лллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллл}

Function SetBPM: Word;
BEGIN
{  CallBpm:=Round(MixRate/(Timing.Bpm*0.4));}
  CallBpm   :=(LongInt(MixRate)*5) div (Word(Timing.Bpm) shl 1);
  CallBpmMod:=(LongInt(MixRate)*5) mod (Word(Timing.Bpm) shl 1);
  CallBpmModCnt:=0; { ? }
  SetBPM:=NoError;
END;

{лллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллл}

Function VSetMode(Voice, Mode: Byte): Word;
BEGIN
  VSetMode:=NoError;
END;

{лллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллл}

Function VGetMode(Voice: Byte): Byte;
BEGIN
  VGetMode:=0;
END;

{лллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллл}

Function VPlay(Voice, Sample: Byte; Offset: Word): Word;
BEGIN
  If (Sample<1) or (Sample>MaxSamples) then exit;
  With MixInfo[Voice] do
  Begin
    RepeatSample:=ModInfo.Samples[Sample].RepeatSample;
    RepeatOffset:=ModInfo.Samples[Sample].RepeatOffset{+1};
    RepeatLength:=ModInfo.Samples[Sample].RepeatLength{+1};
    Length      :=ModInfo.Samples[Sample].Length{+1};
    SampleSeg   :=Seg(SamplePointers[Sample]^);
    SampleOfs   :=Ofs(SamplePointers[Sample]^);
    RealIndex   :=LongInt(Offset{+1}) shl 16;
    OnMix       :=True;
  End;
  VPlay:=NoError;
END;

{лллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллл}

Function VSetPanning(Voice, Panning: Byte): Word;
BEGIN
  With MixInfo[Voice] do
  Begin
    LeftVol :=Byte(Word(Word(255-Panning)*Vol) shr 8);
    RightVol:=Byte(Word(Word(    Panning)*Vol) shr 8);
  End;
  VSetPanning:=NoError;
END;

{лллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллл}

Function VSetVolume(Voice, Volume: Byte): Word;
BEGIN
  If Volume>64 then Volume:=64;
  With MixInfo[Voice] do
  Begin
    Vol     :=Volume;
    LeftVol :=Byte(Word(Word(255-Channels[Voice].Pan)*Vol) shr 8);
    RightVol:=Byte(Word(Word(    Channels[Voice].Pan)*Vol) shr 8);
  End;

  VSetVolume:=NoError;
END;

{лллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллл}

Function VSetFrequency(Voice: Byte; Frequency: LongInt): Word;
BEGIN
{  MixInfo[Voice].IncEr:=Round((Frequency/MixRate)*65536.0); to be tested}
  MixInfo[Voice].IncEr:=((Frequency shl 12) div MixRate) shl 4;
  VSetFrequency:=NoError;
END;

{лллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллл}
{лллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллл}

VAR
  I: Word;
BEGIN
  SW_Detect;
  Addr(SBDevice.LoadMODSample):=Addr(LoadMODSample);
  Addr(SBDevice.HW_Detect    ):=Addr(HW_Detect);
  Addr(SBDevice.OpenCard     ):=Addr(OpenCard);
  Addr(SBDevice.CloseCard    ):=Addr(CloseCard);
  Addr(SBDevice.StartReplay  ):=Addr(StartReplay);
  Addr(SBDevice.StopReplay   ):=Addr(StopReplay);
  Addr(SBDevice.SetBPM       ):=Addr(SetBPM);
  Addr(SBDevice.VSetMode     ):=Addr(VSetMode);
  Addr(SBDevice.VGetMode     ):=Addr(VGetMode);
  Addr(SBDevice.VPlay        ):=Addr(VPlay);
  Addr(SBDevice.VSetPanning  ):=Addr(VSetPanning);
  Addr(SBDevice.VSetVolume   ):=Addr(VSetVolume);
  Addr(SBDevice.VSetFrequency):=Addr(VSetFrequency);
  For I:=1 to MaxSamples do SamplePointers[I]:=Nil;
END.
{$F-}
