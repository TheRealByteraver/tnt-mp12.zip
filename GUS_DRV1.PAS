{
  This is the GUS Driver.
  Protected Mode Ready

  Copyright (c) 1998 Erland Van Olmen (erlandvo@hotmail.com)

þþBugfixes/changes/additions since version 1.0 (first public release):

      - IRQ 0 (int 8) is now chained, so the time get's updated.
        Because of this fix, the GUS replay routines don't work under W95
        anymore (dunno for Windows 3.1).
      - Fixed bug in GUS-click removal: chiptunes sound right now
        (it was a problem related to small sample loop-lengths)


þþBugfixes/changes/additions since version 1.1 (2nd public release):

      - Removed code to chain IRQ 0 (int 8), as it seems to make the
        program instable, especially together with the ShowPatternLine
        Function from MP.PAS
      - Removed floating point calculation

}
{DEFINE DEBUGINFO}
{I-,N+,E-}
Unit GUS_DRV1;

interface

Uses
  MODType1, Replay;

CONST
{$IFDEF DPMI}
  UseDMA = False;
{$ELSE}
  UseDMA: Boolean = false {True};
{$ENDIF}

VAR
  GUSDevice: TOutputDevice;

implementation

Uses
  NewCrt,
  Dos,
  HardWare,
  ULTRADRV; { Use GUS SDK driver }

{$F+}
CONST
  MaxSamples = 31;
  TC1        : Word  = 0;
  TC2        : Word  = 0;
  RestCnt    : Word  = 0;

VAR
  SampleLocations : Array[1..MaxSamples] of LongInt;
  GUSPos          : LongInt;
  OldIRQ0Interrupt: Procedure;
  TimerCount      : Byte;
  Chain1, Chain2  : Word;

Procedure GUSPlayer; interrupt;
BEGIN
{
  OldIRQ0Interrupt; Port[$20]:=$20; Exit;  // werkt niet onder windows:
                                           // "illegal instruction"

  asm sti; int 1ch; end; Exit;             // werkt niet onder RAW DOS:
                                           // blijft hangen (hardreset)
}
  asm cli end;

  Inc(TC1); Inc(TC2); Inc(TimerCount);
  If TimerCount>9 then
  Begin
    TimerCount:=0;
    If Timing.Count<255 then Inc(Timing.Count);
    If (Timing.Count>=Timing.Speed) and (Timing.PatternDelay=0) then
         Begin Timing.Count:=0; UpdateNotes; End
    Else UpdateMultipleStepsEffects;
  End;
{
  If TC1>Word(timing.bpm)*40 then begin TC1:=0; Inc(RestCnt, Chain2); End;
  If TC2>Chain1 then
    Begin
      TC2:=0;
      OldIRQ0Interrupt; Port[$20]:=$20; asm sti; int 1ch; end; Exit;
      asm call OldIRQ0Interrupt ; sti end; Port[$20]:=$20; Exit;
    End
  Else

  If RestCnt>182 then
    Begin
      RestCnt:=RestCnt mod 182;
      asm call OldIRQ0Interrupt end;
    End;
}
  Port[$20]:=$20;
  asm sti end;
END;

{This extra intermediate function is needed because the stupid DMA controller
 can't do transfers that cross a Page break and the stupid GUS can't start a
 transfer on anything else than a 16 or 32byte boundary! The GUS SDK should
 consider this problem, actually it seems to be buggy, so...                  }
FUNCTION UltraDownLoad(DataPtr  : POINTER;
                       Control  : BYTE;
                       DRAM_Loc : LONGINT;
                       Len      : WORD;
                       Wait     : BOOLEAN) : BOOLEAN;
VAR
  RetCode    : Boolean;
  K,
  Page1,
  Page2      : Byte;
  PageOffset,
  Temp,
  RealAddress: LongInt;
BEGIN
  RealAddress:=LongInt(Seg(DataPtr^)) shl 4 + Ofs(DataPtr^);
  Page1      := RealAddress      shr 16;
  Page2      :=(RealAddress+len) shr 16;
  If Page1=Page2 then
    Begin
      RetCode:=ULTRADRV.UltraDownLoad(DataPtr, Control, DRAM_Loc, Len, Wait);
      IF RetCode THEN UltraDownLoad:=True
      Else            UltraDownLoad:=False;
    End
  Else
    Begin
      Temp:=LongInt(Page2) shl 16; Dec(Temp, RealAddress);
      Temp:=Temp And ($FFFFFFFF-$1F);

      RetCode:=ULTRADRV.UltraDownLoad(DataPtr, Control, DRAM_Loc, Temp, Wait);
      If Not RetCode Then Begin UltraDownLoad:=False; Exit; End;

      Inc(DRAM_Loc, Temp); Dec(Len, Temp);
      If Len>=32 then
        Begin
          For K:=0 to 31 do
            UltraPokeData(GUSDevice.MPort, DRAM_Loc+K, PByteBuffer(DataPtr)^[Temp+K]);
          Dec(Len, 32); Inc(RealAddress, 32); Inc(DRAM_Loc, 32);
        End
      Else
        Begin
          For K:=0 to Len-1 do
            UltraPokeData(GUSDevice.MPort, DRAM_Loc+K, PByteBuffer(DataPtr)^[Temp+K]);
          UltraDownLoad:=True;
          Exit;
        End;

      Inc(RealAddress, Temp);

      DataPtr:=Ptr(RealAddress shr 4, RealAddress and $F);
      RetCode:=ULTRADRV.UltraDownLoad(DataPtr, Control, DRAM_Loc, Len, Wait);
      IF RetCode THEN UltraDownLoad:=True
      Else            UltraDownLoad:=False;
    End;
END;

Function LoadMODSample(var F: File; SampleNr: Byte): Word;
VAR
  Size,
  Alloc    : LongInt;
  Buf      : PByteBuffer;
  Count,
  I,
  K,
  Result   : Word;
  GUSPage1,
  GUSPage2 : Byte;
BEGIN
  LoadMODSample:=255;
  Inc(GUSPos, 32); {yes, 32, NOT 31! Because!} GUSPos:=GUSPos and ($FFFFFFFF-$1F);
  Size:=ModInfo.Samples[SampleNr].Length;

{Needed for buggy GUS SDK DMA routines! -------------------------------------}
  If UseDMA then
  Begin
    GUSPage1:=GUSPos shr 18; GUSPage2:=(GUSPos+Size) shr 18;
    If GUSPage2<>GUSPage1 then GUSPos:=LongInt(GUSPage2) shl 18;
  End;

  SampleLocations[SampleNr]:=GUSPos;
  Alloc:=MaxAvail; If Alloc<$100 then Begin LoadMODSample:=OutOfMemory; Exit; End;
  If Alloc>$F000 then Alloc:=$F000; Alloc:=Alloc and ($FFFF-$1F);
  GetMem(Buf, Alloc);
  Count:=Size div Alloc;
  If Count<>0 then
    For I:=1 to Count do
    Begin
      BlockRead(F, Buf^, Alloc, Result);
      If UseDMA then UltraDownLoad(Buf, 0, GUSPos, Result, TRUE)
      Else For K:=0 to Result-1 do UltraPokeData(GUSDevice.MPort, GUSPos+K, Buf^[K]);
      Inc(GUSPos, Result); Dec(Size, Result);
      If (Alloc<>Result) then
        Begin
{Correct Sample header info: ------------------------------------------------}
          ModInfo.Samples[SampleNr].Length:=LongInt(I-1)*Alloc+Result;
          LoadMODSample:=OutOfSampleData; FreeMem(Buf, Alloc); Exit;
        End;
    End;

  BlockRead(F, Buf^, Size, Result);
  If UseDMA then UltraDownLoad(Buf, 0, GUSPos, Result, TRUE)
  Else For K:=0 to Result-1 do UltraPokeData(GUSDevice.MPort, GUSPos+K, Buf^[K]);
  Inc(GUSPos, Result);
  If (Size<>Result) then
    Begin
{Correct Sample header info: ------------------------------------------------}
      ModInfo.Samples[SampleNr].Length:=Result;
      LoadMODSample:=OutOfSampleData; FreeMem(Buf, Alloc); Exit;
    End;
{Now go for some stupid useless GUSClick Removal: ---------------------------}
  If ModInfo.Samples[SampleNr].RepeatSample then
    Begin
      If ModInfo.Samples[SampleNr].RepeatOffset=0 then
        Begin
          I:=UltraPeekData(GUSDevice.MPort, SampleLocations[SampleNr]);
          UltraPokeData(GUSDevice.MPort   , SampleLocations[SampleNr]-1, I);
          UltraPokeData(GUSDevice.MPort   , GUSPos, I);
        End
      Else
        Begin
          I:=UltraPeekData(GUSDevice.MPort, SampleLocations[SampleNr]);
          UltraPokeData(GUSDevice.MPort   , SampleLocations[SampleNr]-1, I);
          If (ModInfo.Samples[SampleNr].RepeatOffset +
              ModInfo.Samples[SampleNr].RepeatLength) =
              ModInfo.Samples[SampleNr].Length then
            Begin
              I:=UltraPeekData(GUSDevice.MPort, SampleLocations[SampleNr]+ModInfo.Samples[SampleNr].RepeatOffset);
              UltraPokeData(GUSDevice.MPort   , GUSPos, I);
            End;
        End;
    End
  Else
    Begin
      I:=UltraPeekData(GUSDevice.MPort, SampleLocations[SampleNr]);
      UltraPokeData(GUSDevice.MPort   , SampleLocations[SampleNr]-1, I);
      I:=UltraPeekData(GUSDevice.MPort, GUSPos-1);
      UltraPokeData(GUSDevice.MPort   , GUSPos  , I);
{      UltraPokeData(GUSDevice.MPort   , GUSPos+1, 0);}
    End;
  Inc(GUSPos, 1);
{End of stupid non-functional click-removal... ------------------------------}
{  UltraPokeData(GUSDevice.MPort, SampleLocations[SampleNr]  , 0);
  UltraPokeData(GUSDevice.MPort, SampleLocations[SampleNr]+1, 0);}
  FreeMem(Buf, Alloc);
  LoadMODSample:=NoError;
END;

Function SW_Detect: Boolean;
BEGIN
  With GUSDevice do
  Begin
    SubType:=0;
    ID     :=MODType1.GUSDevice;
    Name   :=DeviceNames[ID];
    MEnvStr:='ULTRASND';
    SEnvStr:='ULTRA16';
    MPort  :=$220;
    SPort  :=$C32;
    MIRQ   :=11;
    SIRQ   :=3;
    MDMA   :=1;
    SDMA   :=6;
    HWDetected:=False;
    SWDetected:=Ultra_Installed;
    SW_Detect:=SWDetected;

{$IFNDEF DPMI}
    If not SWDetected then UseDMA:=False;
{$ENDIF}
  End;
END;

Function HW_Detect: Boolean;
BEGIN
  HW_Detect:=False;
END;

Function OpenCard(NrVoices: Byte): Word;
VAR
  Res,
  I   : Word;
BEGIN
  TimerCount:=0;
  If NrVoices<14 then NrVoices:=14; If NrVoices>32 then NrVoices:=32;
  Res:=Ord(UltraOpen(Ultra_Config, NrVoices));
  OpenCard:=Res;
END;

Function CloseCard: Word;
BEGIN
  UltraClose;
  TimerSpeedUp($FFFF);
  CloseCard:=NoError;
END;

Function StartReplay: Word;
BEGIN
  ResetSong;
  GetIntVec($8, @OldIRQ0Interrupt);
  SetIntVec($8, @GUSPlayer);
  GUSDevice.SetBPM;
  Playing:=True;
  StartReplay:=NoError;
END;

Function StopReplay: Word;
BEGIN
  SetIntVec($8, @OldIRQ0Interrupt);
  StopReplay:=NoError;
END;

Function SetBPM: Word;
VAR
  TC: LongInt;
BEGIN {1193182 or 1192755?}
  Chain1:=(40*Timing.bpm) div 182 + 1;
  Chain2:=(40*Timing.bpm) mod 182;
  TC:=(1192755 div Timing.Bpm) shr 2;
  If TC>$FFFF then TC:=$FFFF; TimerSpeedUp(TC);
  SetBPM:=NoError;
END;

Function VSetMode(Voice, Mode: Byte): Word;
BEGIN
  UltraSetLoopMode(Voice-1, Mode);
  VSetMode:=NoError;
END;

Function VGetMode(Voice: Byte): Byte;
BEGIN
  VGetMode:=1; { not supported }
END;

Function VPlay(Voice, Sample: Byte; Offset: Word): Word;
BEGIN
  If (Sample<1) or (Sample>MaxSamples) then exit;
  UltraStopVoice(Voice-1);
  UltraStartVoice(Voice-1,
    SampleLocations[Sample]+Offset,
    SampleLocations[Sample]+MODInfo.Samples[Sample].RepeatOffset,
    SampleLocations[Sample]+MODInfo.Samples[Sample].Length,
    Ord(MODInfo.Samples[Sample].RepeatSample) shl 3);
  VPlay:=NoError;
END;

Function VSetPanning(Voice, Panning: Byte): Word;
BEGIN
  UltraSetBalance(Voice-1, Panning shr 4);
  VSetPanning:=NoError;
END;

Function VSetVolume(Voice, Volume: Byte): Word;
BEGIN
  If Volume>64 then Volume:=64;
  UltraSetLinearVolume(Voice-1, Word(Volume));
  VSetVolume:=NoError;
END;

Function VSetFrequency(Voice: Byte; Frequency: LongInt): Word;
BEGIN
  UltraSetFrequency(Voice-1, Frequency);
  VSetFrequency:=NoError;
END;

VAR
  I: Word;
BEGIN
  SW_Detect;
  Addr(GUSDevice.LoadMODSample):=Addr(LoadMODSample);
  Addr(GUSDevice.HW_Detect    ):=Addr(HW_Detect);
  Addr(GUSDevice.OpenCard     ):=Addr(OpenCard);
  Addr(GUSDevice.CloseCard    ):=Addr(CloseCard);
  Addr(GUSDevice.StartReplay  ):=Addr(StartReplay);
  Addr(GUSDevice.StopReplay   ):=Addr(StopReplay);
  Addr(GUSDevice.SetBPM       ):=Addr(SetBPM);
  Addr(GUSDevice.VSetMode     ):=Addr(VSetMode);
  Addr(GUSDevice.VGetMode     ):=Addr(VGetMode);
  Addr(GUSDevice.VPlay        ):=Addr(VPlay);
  Addr(GUSDevice.VSetPanning  ):=Addr(VSetPanning);
  Addr(GUSDevice.VSetVolume   ):=Addr(VSetVolume);
  Addr(GUSDevice.VSetFrequency):=Addr(VSetFrequency);
  GUSPos:=32; For I:=1 to MaxSamples do SampleLocations[I]:=0;
END.

