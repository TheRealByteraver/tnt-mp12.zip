{
  Copyright (c) 1998-2009 Erland Van Olmen (erlandvo@hotmail.com)
  Protected Mode Ready

  This is the Pascal equivalent of the external ASM module that contains the
  mixing routines.
  Note that these routines are simplier than the ASM ones: the pascal routine
  has no Volume Zero Optimisation for instance.
  I tried to optimize the code but there is no point in doing that really,
  the compiler generates such unoptimized code that a litteral assembler
  translation will offer a tenfold speed increase at least.
  This is partly due to the fact that the turbo pascal compiler generates 286+
  code, the asm routine uses 386+ code


þþBugfixes/changes/additions since version 1.0 (first public release):

      - None


þþBugfixes/changes/additions since version 1.1 (2nd public release):

      - Removed amplify boundary (range) check from the inner loop. This
        not only makes the mixer faster but also really improves sound
        quality (especially when playing modules with 8 channels and more)
        This is actually such a big improvement that it can really be called
        a bugfix ;)
      - Added Pascal Stereo Mixing routines for the sake of completeness
      - Added CUBIC interpolation (sounds almost the same as linear
        interpolation though).
      - Made the code somewhat smaller en nicer
      - fixed the nasty bug that occured sometimes with MODs that need to
        be played at very low speeds (BPM). Finally!
      - If the "SixteenbitMix" variable is set the mixer will output 16 bit
        data, for slightly better sound quality, especially with low volume
        samples and multichannel modules (more than 8 channels)


  This routine bugs w/ xenolog1.mod... or pattern delay, when using the disk
  writing device the behaviour is not same as when using replay? :s

  The bug occurs ONLY if:
      - the WAV writer is used (NO!)
      - the Mixrate is high    (NO!)
      - the Mixbuffer is big   (NO!)
      - the bug is related to the allocation of the memory of the mix
        buffers...
      --==> this bug should be fixed ****
}

{ The following variables are used only by the mixer: }
VAR  MixIndex, MixLoopCnt, TMixIndex: LongInt;

{
 geval 1: Sample = RepeatSample
   geval a: MixLen <= sample index + freq ctr * MixLen
      -> Mix MixLen bytes, exit
   geval b: MixLen >  sample index + freq ctr * MixLen
      -> repeat
           MixBuf(SDataLen)




}

Procedure MONO_MIX_8BIT(NrB2Mix: Word);
VAR
  chn : Byte;
  smp : PByteBuffer;
  intp: LongInt;
  TmpAmp: Word;

BEGIN
{ Now mix all the channels together: ----------------------------------------}
  For Chn:=1 to ModInfo.NrChannels do
  If MixInfo[chn].OnMix then                        { Should I mix this chn? }
  Begin
    TmpAmp:=Word(Amplify)*MixInfo[chn].Vol;
    MixLoopCnt:=NrB2Mix; TMixIndex:=MixIndex;    { update/init loop counters }
    smp:=ptr(MixInfo[chn].SampleSeg, MixInfo[chn].SampleOfs);
    Repeat
      intp:=MixInfo[chn].RealIndex shr 16 + 1;
      If intp<MixInfo[chn].Length then
        Begin
          Inc(MixInfo[chn].RealIndex, MixInfo[chn].IncEr); {+1 skip intrpltve aide}
          Inc(TMixBuffer^[TMixIndex], TmpAmp*ShortInt(smp^[intp]));
          dec(MixLoopCnt); Inc(TMixIndex);  { increment Index of temp buffer }
        End
      Else
        If MixInfo[chn].RepeatSample then
            MixInfo[chn].RealIndex:=LongInt(MixInfo[chn].RepeatOffset) shl 16
                                         + (MixInfo[chn].RealIndex and $FFFF)
        Else Begin MixInfo[chn].OnMix:=False; MixLoopCnt:=0; End;
    Until MixLoopCnt = 0;
  End;
  Inc(MixIndex, NrB2Mix);
END;

{****************************************************************************}

Procedure MONO_MIX_8BIT_I(NrB2Mix: Word); {interpolates}
VAR
  chn : Byte;
  smp : PByteBuffer;
  y1, y2: Integer;
  intp, fract, Y: LongInt;
  TmpAmp: Word;

BEGIN
{ Now mix all the channels together: ----------------------------------------}
  For Chn:=1 to ModInfo.NrChannels do
  If MixInfo[chn].OnMix then                        { Should I mix this chn? }
  Begin
    TmpAmp:=Amplify*MixInfo[chn].Vol;
    MixLoopCnt:=NrB2Mix; TMixIndex:=MixIndex;    { update/init loop counters }
    smp:=ptr(MixInfo[chn].SampleSeg, MixInfo[chn].SampleOfs);
    Repeat
      intp :=     MixInfo[chn].RealIndex shr 16 + 1;{ keep high  16 bits only }
      fract:=WORD(MixInfo[chn].RealIndex);          { keep lower 16 bits only }
      Inc(MixInfo[chn].RealIndex, MixInfo[chn].IncEr);
      If intp<MixInfo[chn].Length then
        Begin                 { push word on buffer & clean High Word of it: }
          Y1:=ShortInt(smp^[intp  ]);
          Y2:=ShortInt(smp^[intp+1]);
          Y :=Integer((LongInt(Y1) shl 16 + fract * (Y2-Y1)) shr 16);
          Inc(TMixBuffer^[TMixIndex], Y*TmpAmp);
          dec(MixLoopCnt); Inc(TMixIndex);  { increment Index of temp buffer }
        End
      Else
        If MixInfo[chn].RepeatSample then
             MixInfo[chn].RealIndex:=LongInt(MixInfo[chn].RepeatOffset) shl 16
        Else Begin MixInfo[chn].OnMix:=False; MixLoopCnt:=0; End;
    Until MixLoopCnt = 0;
  End;
  Inc(MixIndex, NrB2Mix);
END;

{****************************************************************************}

Procedure MONO_MIX_8BIT_IC(NrB2Mix: Word); { interpolates, cubic }
VAR
  chn : Byte;
  smp : PByteBuffer;
  p0, p1, p2, p3, a, b, c, t: Integer;
  f2, tmp, intp, fract: LongInt;
  d, TmpAmp: Word;
  f: double;

BEGIN
{ Now mix all the channels together: ----------------------------------------}
  For Chn:=1 to ModInfo.NrChannels do
  If MixInfo[chn].OnMix then                        { Should I mix this chn? }
  Begin
    smp:=ptr(MixInfo[chn].SampleSeg, MixInfo[chn].SampleOfs);
    TmpAmp:=Amplify * MixInfo[chn].Vol shr 4; {shr 4 needed 4 fxd pt calc!}
    MixLoopCnt:=NrB2Mix; TMixIndex:=MixIndex;    { update/init loop counters }
    Repeat
      intp :=     MixInfo[chn].RealIndex shr 16 + 1;{ keep high  16 bits only }
      fract:=WORD(MixInfo[chn].RealIndex) shr 8;    { keep lower 16 bits only }
      Inc(MixInfo[chn].RealIndex, MixInfo[chn].IncEr);
      If intp<MixInfo[chn].Length then
        Begin
{          If intp=0 then p0:=p1 Else }p0:=ShortInt(smp^[intp-1]);
          p1:=ShortInt(smp^[intp  ]);
          p2:=ShortInt(smp^[intp+1]);
          p3:=ShortInt(smp^[intp+2]);
{16 bit calculations: }
          t:=p1 - p2;
          a:=(t shl 1 + t - p0 + p3);
          b:=(p2 shl 1 + p0) shl 1 - (p1 shl 2 + p1 + p3);
          c:=(p2 - p0);
{32 bit calculations: here are the original lines of code, for clarity...   }
          f2:=Integer(((LongInt(a) * fract) shl  3  + LongInt(b)  shl 11) div $100);
          f2:=Integer((f2          * fract) div $100) +         c   shl 3;
          f2:=Integer((f2          * fract) div $100) +         p1  shl 4;
          Inc(TMixBuffer^[TMixIndex], f2 * TmpAmp);
{32 bit calculations: } { in 1 expression for better compiler optimisation:
          f2:=Integer(((Integer((
              Integer(((LongInt(a) * fract) shl  3  + LongInt(b)  shl 19) shr 16)
              * fract) shr 16) + LongInt(c)  shl 3)
              * fract) shr 16) + LongInt(p1) shl 4;
          Inc(TMixBuffer^[TMixIndex], Integer(f2) * TmpAmp);
{64 bit floating point calculations: - floating point alternative,
 the fastest way to do it in 286+ pascal:
          t:=p1 - p2;
          a:=(t shl 1 + t - p0 + p3) div 2;
          b:=p2 shl 1 + p0 - (p1 shl 2 + p1 + p3) div 2;
          c:=(p2 - p0) div 2;
          f:=fract/$10000;
          tmp:=Round((((a * f + b) * f) + c) * f + p1) * TmpAmp;
          Inc(TMixBuffer^[TMixIndex], Tmp); {}
          dec(MixLoopCnt); Inc(TMixIndex);  { increment Index of temp buffer }
        End
      Else
        If MixInfo[chn].RepeatSample then
           MixInfo[chn].RealIndex:=LongInt(MixInfo[chn].RepeatOffset) shl 16
        Else Begin MixInfo[chn].OnMix:=False; MixLoopCnt:=0; End;
    Until MixLoopCnt = 0;
  End;
  Inc(MixIndex, NrB2Mix);
END;

{****************************************************************************}

Procedure _UPDATEBPM;
BEGIN
  Inc(Timing.count);                             { Yes, update bpm (and FX)}
  if (Timing.count<Timing.speed) or (Timing.PatternDelay>0) then
                                                   UpDateMultipleStepsEffects
  Else Begin WaitState:=True; Timing.Count:=0; UpDateNotes; End;
END;

{****************************************************************************}

Procedure STEREO_MIX_8BIT(NrB2Mix: Word);
VAR
  chn: Byte;
  intp, tmp: LongInt;
  smp: PByteBuffer;
  d, TmpAmpL, TmpAmpR: Word;

BEGIN
{ Now mix all the channels together: ----------------------------------------}
  For Chn:=1 to ModInfo.NrChannels do
  If MixInfo[chn].OnMix then                        { Should I mix this chn? }
  Begin
    TmpAmpL:=Word(Amplify)*MixInfo[chn].LeftVol;
    TmpAmpR:=Word(Amplify)*MixInfo[chn].RightVol;
    MixLoopCnt:=NrB2Mix; TMixIndex:=MixIndex;    { update/init loop counters }
    smp:=ptr(MixInfo[chn].SampleSeg, MixInfo[chn].SampleOfs);
    Repeat
      intp :=MixInfo[chn].RealIndex shr 16 + 1;   { keep high  16 bits only }
      Inc(MixInfo[chn].RealIndex, MixInfo[chn].IncEr);
      If intp<MixInfo[chn].Length then
        Begin
          tmp:=ShortInt(smp^[intp]);
          Inc(TMixBuffer^[TMixIndex], tmp*TmpAmpL); Inc(TMixIndex);{ inc tmp buf idx }
          Inc(TMixBuffer^[TMixIndex], tmp*TmpAmpR); Inc(TMixIndex);{ inc tmp buf idx }
          dec(MixLoopCnt);
        End
      Else
        If MixInfo[chn].RepeatSample then
             MixInfo[chn].RealIndex:=LongInt(MixInfo[chn].RepeatOffset) shl 16
        Else Begin MixInfo[chn].OnMix:=False; MixLoopCnt:=0; End;
    Until MixLoopCnt = 0;
  End;
  Inc(MixIndex, NrB2Mix*2);
END;

{****************************************************************************}

Procedure STEREO_MIX_8BIT_I(NrB2Mix: Word); {interpolates}
VAR
  chn : Byte;
  smp : PByteBuffer;
  y1, y2: Integer;
  tmp, Y, intp, fract: LongInt;
  TmpAmpL, TmpAmpR: Word;

BEGIN
{ Now mix all the channels together: ----------------------------------------}
  For Chn:=1 to ModInfo.NrChannels do
  If MixInfo[chn].OnMix then                        { Should I mix this chn? }
  Begin
    TmpAmpL:=Word(Amplify)*MixInfo[chn].LeftVol;
    TmpAmpR:=Word(Amplify)*MixInfo[chn].RightVol;
    MixLoopCnt:=NrB2Mix; TMixIndex:=MixIndex;    { update/init loop counters }
    smp:=ptr(MixInfo[chn].SampleSeg, MixInfo[chn].SampleOfs);
    Repeat
      intp :=     MixInfo[chn].RealIndex shr 16 + 1;{ keep high  16 bits only }
      fract:=WORD(MixInfo[chn].RealIndex);          { keep lower 16 bits only }
      Inc(MixInfo[chn].RealIndex, MixInfo[chn].IncEr);
      If intp<MixInfo[chn].Length then
        Begin                 { push word on buffer & clean High Word of it: }
          Y1:=ShortInt(smp^[intp  ]);
          Y2:=ShortInt(smp^[intp+1]);
          Y :=Integer((LongInt(Y1) shl 16 + fract * (Y2-Y1)) shr 16);
          Inc(TMixBuffer^[TMixIndex], Y*TmpAmpL); Inc(TMixIndex); { inc tmp buf idx }
          Inc(TMixBuffer^[TMixIndex], Y*TmpAmpR); Inc(TMixIndex); { inc tmp buf idx }
          dec(MixLoopCnt);
        End
      Else
        If MixInfo[chn].RepeatSample then
             MixInfo[chn].RealIndex:=LongInt(MixInfo[chn].RepeatOffset) shl 16
        Else Begin MixInfo[chn].OnMix:=False; MixLoopCnt:=0; End;
    Until MixLoopCnt = 0;
  End;
  Inc(MixIndex, NrB2Mix*2);
END;

{****************************************************************************}

Procedure STEREO_MIX_8BIT_IC(NrB2Mix: Word); {interpolates, cubic}
VAR
  chn : Byte;
  smp : PByteBuffer;
  p0, p1, p2, p3, a, b, c, t: Integer;
  f2, tmp, intp, fract: LongInt;
  TmpAmpL, TmpAmpR: Word;
  f: double;

BEGIN
{ Now mix all the channels together: ----------------------------------------}
  For Chn:=1 to ModInfo.NrChannels do
  If MixInfo[chn].OnMix then                        { Should I mix this chn? }
  Begin
    TmpAmpL:=Word(Amplify)*MixInfo[chn].LeftVol;
    TmpAmpR:=Word(Amplify)*MixInfo[chn].RightVol;
    MixLoopCnt:=NrB2Mix; TMixIndex:=MixIndex;    { update/init loop counters }
    smp:=ptr(MixInfo[chn].SampleSeg, MixInfo[chn].SampleOfs);
    Repeat
      intp :=     MixInfo[chn].RealIndex shr 16 + 1;{ keep high  16 bits only }
      fract:=WORD(MixInfo[chn].RealIndex);          { keep lower 16 bits only }
      Inc(MixInfo[chn].RealIndex, MixInfo[chn].IncEr);
      If intp<MixInfo[chn].Length then
        Begin                 { push word on buffer & clean High Word of it: }
          p1:=ShortInt(smp^[intp  ]);
          p2:=ShortInt(smp^[intp+1]);
          p3:=ShortInt(smp^[intp+2]);
          {If intp=0 then p0:=p1 Else} p0:=ShortInt(smp^[intp-1]);
{16 bit calculations: }
          t:=p1 - p2;
          a:=(t shl 1 + t - p0 + p3) div 2;
          b:=p2 shl 1 + p0  - (p1 shl 2 + p1 + p3) div 2;
          c:=(p2 - p0) div 2;
{32 bit calculations:
          f2:=((LongInt(a) * fract)         + LongInt(b)  shl 16) div $1000;
          f2:= Integer((f2 * fract) shr 16) + LongInt(c)  shl 4;
          f2:= Integer((f2 * fract) shr 16) + LongInt(p1) shl 4;
          tmp:=(f2 * TmpAmpL) div $10;
{64 bit floating point calculations: - floating point alternative: }
          f:=fract/$10000;
          tmp:=Round((((a * f + b) * f) + c) * f) + p1;
          Inc(TMixBuffer^[TMixIndex], tmp*TmpAmpL); Inc(TMixIndex); { inc Idx tmp buf }
{          tmp:=(f2 * TmpAmpR) div $10;}
          Inc(TMixBuffer^[TMixIndex], tmp*TmpAmpR); Inc(TMixIndex); { inc Idx tmp buf }
          dec(MixLoopCnt);
        End
      Else
        If MixInfo[chn].RepeatSample then
             MixInfo[chn].RealIndex:=LongInt(MixInfo[chn].RepeatOffset) shl 16
        Else Begin MixInfo[chn].OnMix:=False; MixLoopCnt:=0; End;
    Until MixLoopCnt = 0;
  End;
  Inc(MixIndex, NrB2Mix*2);
END;

{****************************************************************************}

Procedure DownScaleBuffer;
Var
  limit: word;
  tmp: LongInt;
Begin
{ And now modify the mixed buf. so we can play it: (put res. downto 8/16bit) }
  Saturation:=0;
  If SixteenBitMix then
    Begin
      For TMixIndex:=0 to MixBufLen div 2 - 1 do
      Begin
        tmp:=TMixBuffer^[TMixIndex];
        if      tmp>MaxA then begin tmp:=MaxA; inc(Saturation); end
        else if tmp<MinA then begin tmp:=MinA; inc(Saturation); end;
{$IFDEF DPMI}
        PIntBuffer (MixBuffer.pm)^[TMixIndex]:=Integer(tmp div 4);
{$ELSE}
        PIntBuffer (MixBuffer)   ^[TMixIndex]:=Integer(tmp div 4);
{$ENDIF}
      End;
    End
  Else
    Begin
      For TMixIndex:=0 to MixBufLen-1 do
      Begin
        tmp:=TMixBuffer^[TMixIndex];
        if      tmp>MaxA then begin tmp:=MaxA; inc(Saturation); end
        else if tmp<MinA then begin tmp:=MinA; inc(Saturation); end;
{$IFDEF DPMI}                                               { 1024 = 64 * 16 }
        PByteBuffer(MixBuffer.pm)^[TMixIndex]:=ShortInt(tmp div 1024) XOR 128;
{$ELSE}
                    MixBuffer    ^[TMixIndex]:=ShortInt(tmp div 1024) XOR 128;
{$ENDIF}
      End;
    End;
End;

{****************************************************************************}

Procedure ChooseMixer(NrB2Mix: Word);
Begin
 If StereoReplay then
    Case Interpolation of
      NoFilter,
      HWFilter: STEREO_MIX_8BIT   (NrB2Mix);
      LIFilter: STEREO_MIX_8BIT_I (NrB2Mix);
      CIFilter: STEREO_MIX_8BIT_IC(NrB2Mix);
    end
  Else
    Case Interpolation of
      NoFilter,
      HWFilter: MONO_MIX_8BIT   (NrB2Mix);
      LIFilter: MONO_MIX_8BIT_I (NrB2Mix);
      CIFilter: MONO_MIX_8BIT_IC(NrB2Mix);
    End;
End;

{****************************************************************************}

Procedure _SB_MIXER;
VAR
  X, Y: LongInt;
BEGIN
  For TMixIndex:=0 to MixBufLen-1 do TMixBuffer^[TMixIndex]:=0;{Clean the buf}
  MixIndex:=0;
  X:=CallBpm-MixCount;
  If StereoReplay  then Y:=MixBuflen div 2 Else Y:=MixBuflen;
  If SixteenBitMix then Y:=Y div 2;
  If X>Y then Begin Inc(MixCount, Y); ChooseMixer(Y); End
  Else
    Begin
      ChooseMixer(X); X:=Y-X; MixCount:=0;
      _UPDATEBPM;
      While X>=Callbpm do
        Begin
          ChooseMixer(CallBpm); Dec(X, CallBpm);
          _UPDATEBPM;
        End;
      If X<>0 then Begin MixCount:=X; ChooseMixer(X); End;
    End;
  DownScaleBuffer;
END;

{****************************************************************************}
{****************************************************************************}

