;%===========================================================================%
;%===========================================================================%
;
;  Copyright (c) 1998 Erland Van Olmen (erlandvo@hotmail.com)
;  Seems to work in Protected Mode
;
;þþBugfixes/changes/additions since version 1.0 (first public release):
;
;      - Fixed The Interpolative 8bit stereo mixer (it did not work before)
;
;
;þþBugfixes/changes/additions since version 1.1 (second public release):
;
;      - Reduced size of the regular 8bit stereo mixer (it's like the other
;        mixers now)
;      - Removed amplify boundary (range) check from the inner loop. This
;        not only makes the mixer faster but also really improves sound
;        quality (especially when playing modules with 8 channels and more)
;        This is actually such a big improvement that it can really be called
;        a bugfix ;)
;
; W-A-R-N-I-N-G ! Please note that:
;
;               - The mono routines don't mix channels if their volume is 0!
;               - The mixer supports full panning so mixing in stereo needs
;                 twice the CPU power of the mono mixer! The only way to avoid
;                 this would be to use maximum stereo separation ( -p0 command
;                 line option) and implement volume 0 optimisations (they are
;                 only implemented in the mono mixing routines)
;
;
; Compile with TASM: "TASM /MU /ZN /OS DMA_MIX?.ASM"
; (press SHIFT+F3 in your Borland Pascal IDE)
;
; PS: sorry for tha loooong labels ;-) !
;
;%===========================================================================%
;%===========================================================================%
;
; -----------------------------------------
;
; Now let's go for some obvious optimisations to come. Here is the inner
; loop (the one that is executed almost 45454 times/sec):
;
;@@_MixLoopStart:
;  mov     ebx, edx
;  add     edx, [ds:si+15]
;  shr     ebx, 16
;  cmp     bx, [ds:si+9]   ; these two instructions check if I reached the end
;  jae   @@_Check4Repeat   ; of the sample. They can be put outside the loop!
;  mov     al, [es:di+bx]  ; the instructions take 2 + 1 = 3 clock ticks (486)
;  mul     [BYTE ds:si+1]
;  mov     [fs:ecx], eax
;  add     ecx, 4
;  dec     [MixLoopCnt]
;  jnz   @@_MixLoopStart
;
;%===========================================================================%
;%===========================================================================%


        IDEAL
        P386

MIN_VOL  EQU  0

SEGMENT DATA word public

EXTRN       StereoReplay: BYTE, Interpolation: BYTE
EXTRN       Timing: BYTE, WaitState: BYTE, ModInfo: BYTE
EXTRN       MixCount: WORD, CallBPM: WORD, MixInfo_Size: WORD, Amplify: WORD
EXTRN       MixInfo: WORD, MixBufLen: WORD
EXTRN       MinA: DWORD, MaxA: DWORD
EXTRN       MixBuffer: FAR PTR, PlayBuffer: FAR PTR, TMixBuffer: FAR PTR


MixIndex    dw      ?
NrB2Mix     dw      ?     ; this word is a parameter to the proc _MIX
X           dw      ?
MixLoopCnt  dw      ?
TmpAmp      dw      ?     ; Amplify * volume
TmpAmpL     dw      ?     ; Amplify * Left  Volume
TmpAmpR     dw      ?     ; Amplify * Right Volume
dummy       dw      ?
frac        dw      ?
y1          dw      ?
y1delta     dw      ?

ChnCnt      db      ?
word_align  db      ?     ; to keep word alignment intact in main prog.

ENDS DATA


SEGMENT CODE byte public

ASSUME  cs:CODE, ds:DATA

PUBLIC  _UPDATEBPM
PUBLIC  _SB_MIXER                              ; big daddy ;)
PUBLIC  _SBMONO_MIXER  , _SBSTEREO_MIXER       ; standard 8 bit mixers
PUBLIC  _SBMONO_MIXER_I, _SBSTEREO_MIXER_I     ; interpolative 8 bit mixers

;PUBLIC  _SWAPBUFFERS
EXTRN   _SWAPBUFFERS
EXTRN   SBUpdateMultipleStepsEffects: NEAR, SBUpdateNotes: NEAR


; %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
; This is the updateBPM proc:
; %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


PROC _UPDATEBPM NEAR
  inc     [Timing]             ; Yes, update bpm (and FX)
  mov     al, [Timing]
  cmp     al, [Timing+2]       ; BpmCount = Speed ?
  jb    @@Else
  mov     ax, [WORD Timing+4]  ; ax = Timing.PatternDelay
  cmp     ax, 0
  jnz   @@Else
  call    SBUpDateNotes        ; Yes, update note info & FX
  inc     [Waitstate]
  mov     [Timing], 0          ; Timing.count = 0
  ret
@@Else:                        ; Update FX only (vibrato, arpeggio, ...):
  call    SBUpDateMultipleStepsEffects
  ret
ENDP _UPDATEBPM


; %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
; This is the PROC that swaps the two external buffers:
; %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


;PROC _SWAPBUFFERS NEAR
;  mov      eax, [MixBuffer ]   ; swap mix & play buffers
;  mov      ebx, [PlayBuffer]
;  mov      [MixBuffer ], ebx
;  mov      [PlayBuffer], eax
;  ret
;ENDP _SWAPBUFFERS


; %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
; This is the mono mixer: 8bit, without interpolation
; %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

PROC MONO_MIX_8BIT NEAR
  push    bp
  mov     al, [BYTE ModInfo]
  mov     [ChnCnt], al         ; nr of channels the second part has to mix

; init segment regs to point to the 2 buffers: ------------------------------
  lfs     di, [TMixBuffer]     ; fs:0 = TMixBuffer^[0]  N-O-T !!!! ofs != 0(?)
  lgs     di, [MixBuffer]      ; gs:0 = MixBuffer^[0]   N-O-T !!!! ofs != 0(?)
  mov     si, OFFSET MixInfo   ; Init si w/ the address of the variable
  sub     si, [MixInfo_Size]

; ***************************************************************************
; First Clean up the temporary DWORD buffer:
; ***************************************************************************

  les     di, [TMixBuffer]
  xor     di, di               ; Hum... BUT NEEDED!
  xor     eax, eax
  mov     cx, [nrB2Mix]        ; NrB2Mix times
  cld                          ; forward; inc di
  rep     stosd                ; fill es:di with zeros

; ***************************************************************************
; Now mix all the channels together:
; ***************************************************************************

MO_MIX_CHANNELS:
  cmp     [ChnCnt], 0
  jz      MO_FINISH_MIXING
  dec     [ChnCnt]             ; decrement loop counter
  add     si, [MixInfo_Size]
  cmp     [BYTE ds:si], 0      ; Should I mix this channel?
  jz      MO_MIX_CHANNELS      ; no, skip channel

; Calculate some base addresses & var's: ------------------------------------

  mov     ax, [Amplify]        ; precalc amplify (ampli = max 127)
  mul     [BYTE ds:si+1]       ; mul by volume   (vol   = max 64)
  mov     [TmpAmp], ax

  mov     ax, [NrB2Mix]        ; init loop counters
  mov     [MixLoopCnt], ax
  mov     es , [ds:si+11]      ; es  = seg(samples[Instr]^)
  mov     di , [ds:si+13]      ; di  = Ofs(samples[Instr]^)
  mov     edx, [ds:si+19]      ; edx = RealIndex
  xor     ecx, ecx             ; cx  = TMixIndex
  xor     eax, eax             ; only do it once!

; start of Vol0 optimisation code
  cmp     [BYTE ds:si+1], MIN_VOL ; if volume < MIN_VOL -=> don't mix!
  jae   @@_StartMixing
  mov     ax, [MixLoopCnt]     ; nr of loops
  mul     [DWORD ds:si+15]     ; multiply w/ IncEr
  add     eax, [ds:si+19]      ; eax:edx = total IncEr; adjust w/ RealIndex
  adc     edx, 0               ; add carrier
  mov     bx, [ds:si+9]        ; bx = length
  shl     ebx, 16              ; ebx = length << 16
  or      edx, edx             ; edx = 0 ?
  jnz   @@_GoForAdjust
  cmp     eax, ebx
  jae   @@_GoForAdjust
  mov     [ds:si+19], eax      ; adjust RealIndex value
  jmp     MO_MIX_CHANNELS
@@_GoForAdjust:
  cmp     [BYTE ds:si+4], 0    ; [ds:di+4] = RepeatSample
  jnz   @@_RepLenDivide
  mov     [BYTE ds:si], 0      ; OnMix = False, stop mixing (no repeat)
  jmp     MO_MIX_CHANNELS
@@_RepLenDivide:
  mov     bx, [ds:si+5]        ; ebx = Repeat Offset
  shl     ebx, 16              ;                      << 16
  sub     eax, ebx             ; substract Repeat Offset
  sbb     edx, 0               ; complete substraction
  mov     bx, [ds:si+7]        ; ebx = repeat Length
  shl     ebx, 16              ;                      << 16
  div     ebx                  ; divide by repeat length;
  mov     bx, [ds:si+5]        ; ebx = Repeat Offset
  shl     ebx, 16              ;                      << 16
  add     edx, ebx             ; add Repeat Offset
  mov     [ds:si+19], edx      ; put rest into RealIndex
  xor     cx, cx               ; set temp buffer index to zero
  jmp     MO_MIX_CHANNELS
; end of Vol0 opti code.

@@_StartMixing:
  mov     ebp, edx             ; ebp = RealIndex

@@_MixLoopStart:               ; Start of Loop
  mov     ebx, ebp
  add     ebp, [ds:si+15]      ; increment sample data index w/ freq inc'er
  shr     bx, 1                ; get rid of a bit ;-)                  2
  mov     [frac], bx           ;                                       1
  shr     ebx, 16
  cmp     bx, [ds:si+9]        ; check if I have to re-mix
  jae   @@_Check4Repeat

  mov     al, [es:di+bx]       ; get byte from sample data             1
  cbw
  imul    [TmpAmp]             ; adjust w/ amplify & volume
  shl     edx, 16              ; convert ax:dx to edx
  mov     dx,  ax              ;

  add     [fs:ecx], edx
  add     ecx, 4
  dec     [MixLoopCnt]
  jnz   @@_MixLoopStart
  mov     [ds:si+19], ebp
  jmp     MO_MIX_CHANNELS      ; don't change the rest of the buffer

@@_Check4Repeat:
  cmp     [BYTE ds:si+4], 0    ; [ds:di+4] = RepeatSample
  jz    @@_NoSampleRepeat      ; Don't repeat sample
  push    bp
  mov     bp, [ds:si+5]        ; edx = RepeatOffset
  shl     ebp, 16              ;                     * $10000
  pop     bp
  jmp   @@_MixLoopStart        ; continue Mixing
@@_NoSampleRepeat:
  mov     [BYTE ds:si], 0      ; OnMix = False
  jmp     MO_MIX_CHANNELS

; ***************************************************************************

MO_FINISH_MIXING:
  xor     bx, bx
  mov     cx, [NrB2Mix]        ; NrB2Mix != 0 !!!
  mov     di, [MixIndex]
  add     [MixIndex], cx       ; YES! Needed, do NOT remove!

@@_3_EndLoop:
  mov     eax, [fs:bx]

  cmp     eax, [MinA]          ; check boundaries here!
  jnl   @@_CheckMaxA           ; MinA:=LongInt(-128*64*16*ModInfo.NrChannels)
  mov     eax, [MinA]          ; MaxA:=LongInt( 127*64*16*ModInfo.NrChannels)
  jmp   @@_EndAmpCheck
@@_CheckMaxA:
  cmp     eax, [MaxA]
  jng   @@_EndAmpCheck
  mov     eax, [MaxA]
@@_EndAmpCheck:

  sar     eax, 10              ; adjust: div by max. volume (64) & amp (16)
  idiv    [BYTE ModInfo]       ; div by chn
  xor     al, 80h              ; SoundBlaster needs unsigned data!
  mov     [gs:di], al          ; al = Mixed byte, push to buffer
  inc     di
  add     bx, 4                ; increment Index of temp buffer
  dec     cx
  jnz   @@_3_EndLoop

@@FINISHED_MIXING:
  pop     bp
  ret
ENDP MONO_MIX_8BIT


; %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
; This is the mono mixer: 8 bit, with interpolation
; %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

PROC MONO_MIX_8BIT_I NEAR
  push    bp
  mov     al, [BYTE ModInfo]
  mov     [ChnCnt], al         ; nr of channels the second part has to mix

; init segment regs to point to the 2 buffers: ------------------------------
  lfs     di, [TMixBuffer]     ; fs:0 = TMixBuffer^[0]  N-O-T !!!! ofs != 0(?)
  lgs     di, [MixBuffer]      ; gs:0 = MixBuffer^[0]   N-O-T !!!! ofs != 0(?)
  mov     si, OFFSET MixInfo   ; Init si w/ the address of the variable
  sub     si, [MixInfo_Size]

; ***************************************************************************
; First Clean up the temporary DWORD buffer:
; ***************************************************************************

  les     di, [TMixBuffer]
  xor     di, di               ; Hum... BUT NEEDED!
  xor     eax, eax
  mov     cx, [nrB2Mix]        ; NrB2Mix times
  cld                          ; forward; inc di
  rep     stosd                ; fill es:di with zeros

; ***************************************************************************
; Now mix all the channels together:
; ***************************************************************************

MO_MIX_CHANNELS_I:
  cmp     [ChnCnt], 0
  jz      MO_FINISH_MIXING_I
  dec     [ChnCnt]             ; decrement loop counter
  add     si, [MixInfo_Size]
  cmp     [BYTE ds:si], 0      ; Should I mix this channel?
  jz      MO_MIX_CHANNELS_I    ; no, skip channel

; Calculate some base addresses & var's: ------------------------------------

  mov     ax, [Amplify]        ; precalc amplify (ampli = max 64)
  mul     [BYTE ds:si+1]       ; mul by volume   (vol   = max 64)
  mov     [TmpAmp], ax

  mov     ax, [NrB2Mix]        ; init loop counters
  mov     [MixLoopCnt], ax
  mov     es , [ds:si+11]      ; es  = seg(samples[Instr]^)
  mov     di , [ds:si+13]      ; di  = Ofs(samples[Instr]^)
  mov     edx, [ds:si+19]      ; edx = RealIndex
  xor     ecx, ecx             ; cx  = TMixIndex
  xor     eax, eax             ; only do it once!

; start of Vol0 optimisation code
  cmp     [BYTE ds:si+1], MIN_VOL ; if volume < MIN_VOL -=> don't mix!
  jae   @@_StartMixing
  mov     ax, [MixLoopCnt]     ; nr of loops
  mul     [DWORD ds:si+15]     ; multiply w/ IncEr
  add     eax, [ds:si+19]      ; eax:edx = total IncEr; adjust w/ RealIndex
  adc     edx, 0               ; add carrier
  mov     bx, [ds:si+9]        ; bx = length
  shl     ebx, 16              ; ebx = length << 16
  or      edx, edx             ; edx = 0 ?
  jnz   @@_GoForAdjust
  cmp     eax, ebx
  jae   @@_GoForAdjust
  mov     [ds:si+19], eax      ; adjust RealIndex value
  jmp     MO_MIX_CHANNELS_I
@@_GoForAdjust:
  cmp     [BYTE ds:si+4], 0    ; [ds:di+4] = RepeatSample
  jnz   @@_RepLenDivide
  mov     [BYTE ds:si], 0      ; OnMix = False, stop mixing (no repeat)
  jmp     MO_MIX_CHANNELS_I
@@_RepLenDivide:
  mov     bx, [ds:si+5]        ; ebx = Repeat Offset
  shl     ebx, 16              ;                      << 16
  sub     eax, ebx             ; substract Repeat Offset
  sbb     edx, 0               ; complete substraction
  mov     bx, [ds:si+7]        ; ebx = repeat Length
  shl     ebx, 16              ;                      << 16
  div     ebx                  ; divide by repeat length;
  mov     bx, [ds:si+5]        ; ebx = Repeat Offset
  shl     ebx, 16              ;                      << 16
  add     edx, ebx             ; add Repeat Offset
  mov     [ds:si+19], edx      ; put rest into RealIndex
  xor     cx, cx               ; set temp buffer index to zero
  jmp     MO_MIX_CHANNELS_I
; end of Vol0 opti code.

@@_StartMixing:
  mov     ebp, edx             ; ebp = RealIndex

@@_MixLoopStart:               ; Start of Loop
  mov     ebx, ebp
  add     ebp, [ds:si+15]      ; increment sample data index w/ freq inc'er
  shr     bx, 1                ; get rid of a bit ;-)                  2
  mov     [frac], bx           ;                                       1
  shr     ebx, 16
;  dec     bx
  cmp     bx, [ds:si+9]        ; check if I have to re-mix
  jae   @@_Check4Repeat

  mov     ax, [es:di+bx]       ; get bytes from sample data             1
  mov     bh, ah
  cbw                          ; make a 16 bit integer                 3
  mov     [y1], ax             ; save it for later                     1
  mov     al, bh
;  mov     al, [es:di+bx+1]     ; get next byte from sample data        1(?)
  cbw                          ; make a 16 bit integer                 3,1

  sub     ax, [y1]             ; calc delta: ax == Yd == Y2-Y1; -255<Yd<+255
  imul    [frac]               ; A=frac*Yd                             26
  shl     edx, 16              ;                                       2
  mov     dx, ax               ; dx:ax -> edx                          1
  mov     ax, [y1]             ; al == y1                              1
  shl     eax, 17              ;                                       2
  sar     eax, 2               ;                                       2
  add     eax, edx             ;                                       1, 2
  sar     eax, 15              ; total == 52 clocks for interpol. routine

  imul    [TmpAmp]             ; adjust w/ amplify & volume
  shl     edx, 16              ; convert ax:dx to edx
  mov     dx,  ax              ;

  add     [fs:ecx], edx
  add     ecx, 4
  dec     [MixLoopCnt]
  jnz   @@_MixLoopStart
  mov     [ds:si+19], ebp
  jmp     MO_MIX_CHANNELS_I    ; don't change the rest of the buffer

@@_Check4Repeat:
  cmp     [BYTE ds:si+4], 0    ; [ds:di+4] = RepeatSample
  jz    @@_NoSampleRepeat      ; Don't repeat sample
;  push    bp
  mov     bp, [ds:si+5]        ; edx = RepeatOffset
  shl     ebp, 16              ;                     * $10000
;  pop     bp
  jmp   @@_MixLoopStart        ; continue Mixing
@@_NoSampleRepeat:
  mov     [BYTE ds:si], 0      ; OnMix = False
  jmp     MO_MIX_CHANNELS_I

; ***************************************************************************

MO_FINISH_MIXING_I:
  xor     bx, bx
  mov     cx, [NrB2Mix]        ; NrB2Mix != 0 !!!
  mov     di, [MixIndex]
  add     [MixIndex], cx       ; YES! Needed, do NOT remove!

@@_3_EndLoop:
  mov     eax, [fs:bx]

  cmp     eax, [MinA]          ; check boundaries here!
  jnl   @@_CheckMaxA           ; MinA:=LongInt(-128*64*16*ModInfo.NrChannels)
  mov     eax, [MinA]          ; MaxA:=LongInt( 127*64*16*ModInfo.NrChannels)
  jmp   @@_EndAmpCheck
@@_CheckMaxA:
  cmp     eax, [MaxA]
  jng   @@_EndAmpCheck
  mov     eax, [MaxA]
@@_EndAmpCheck:

  sar     eax, 10              ; adjust: div by max. volume (64) & amp (16)
  idiv    [BYTE ModInfo]       ; div by chn
  xor     al, 80h              ; SoundBlaster needs unsigned data!
  mov     [gs:di], al          ; al = Mixed byte, push to buffer
  inc     di
  add     bx, 4                ; increment Index of temp buffer
  dec     cx
  jnz   @@_3_EndLoop

@@FINISHED_MIXING:
  pop     bp
  ret
ENDP MONO_MIX_8BIT_I


; %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
; This proc controls the mono mixer:
; %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


PROC _SBMONO_MIXER NEAR
  mov      [MixIndex], 0       ; initialise mixing counter
  mov      ax, [CallBpm]
  mov      bx, [MixCount]
  mov      cx, [MixBuflen]
  sub      ax, bx
  mov      [X], ax             ; X = CallBpm - MixCount
  cmp      ax, cx              ; X <= MixBuflen ?
  jna    @@X_EQUAL_OR_LOWER

@@X_BIGGER:
  add      bx, cx
  mov      [MixCount], bx      ; MixCount = MixCount + MixBuflen
  mov      [NrB2Mix], cx       ; init parameter of the _MIX procedure
  call     MONO_MIX_8BIT       ; mix Mixbuflen times
  ret                          ; Ok, exit

@@X_EQUAL_OR_LOWER:
  sub      cx, ax
  mov      [X], cx             ; X = MixBufLen - X
  mov      [NrB2Mix], ax       ; init parameter of the _MIX procedure
  call     MONO_MIX_8BIT       ; mix X times
  mov      [MixCount], 0       ; reset mix counter
  call     _UPDATEBPM          ; update bpm
  mov      ax, [X]
  cmp      ax, [Callbpm]
  jb     @@Process_Zb

@@Za_Loop:
  mov      cx, [CallBpm]
  mov      [NrB2Mix], cx       ; init parameter of the _MIX procedure
  sub      [X], cx
  call     MONO_MIX_8BIT       ; mix CallBpm times
  call     _UPDATEBPM          ; update bpm
  mov      ax, [X]
  cmp      ax, [Callbpm]
  jnb    @@Za_Loop

@@Process_Zb:
  or       ax, ax
  jz     @@EndProc
  mov      [MixCount], ax      ; init MixCount value
  mov      [NrB2Mix], ax       ; init parameter of the _MIX procedure
  call     MONO_MIX_8BIT       ; mix Mixbuflen times

@@EndProc:
  ret
ENDP _SBMONO_MIXER


; %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
; This PROC controls the mono interpolative mixer:
; %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


PROC _SBMONO_MIXER_I NEAR
  mov      [MixIndex], 0       ; initialise mixing counter
  mov      ax, [CallBpm]
  mov      bx, [MixCount]
  mov      cx, [MixBuflen]
  sub      ax, bx
  mov      [X], ax             ; X = CallBpm - MixCount
  cmp      ax, cx              ; X <= MixBuflen ?
  jna    @@X_EQUAL_OR_LOWER

@@X_BIGGER:
  add      bx, cx
  mov      [MixCount], bx      ; MixCount = MixCount + MixBuflen
  mov      [NrB2Mix], cx       ; init parameter of the _MIX procedure
  call     MONO_MIX_8BIT_I     ; mix Mixbuflen times
  ret                          ; Ok, exit

@@X_EQUAL_OR_LOWER:
  sub      cx, ax
  mov      [X], cx             ; X = MixBufLen - X
  mov      [NrB2Mix], ax       ; init parameter of the _MIX procedure
  call     MONO_MIX_8BIT_I     ; mix X times
  mov      [MixCount], 0       ; reset mix counter
  call     _UPDATEBPM          ; update bpm
  mov      ax, [X]
  cmp      ax, [Callbpm]
  jb     @@Process_Zb

@@Za_Loop:
  mov      cx, [CallBpm]
  mov      [NrB2Mix], cx       ; init parameter of the _MIX procedure
  sub      [X], cx
  call     MONO_MIX_8BIT_I     ; mix CallBpm times
  call     _UPDATEBPM          ; update bpm
  mov      ax, [X]
  cmp      ax, [Callbpm]
  jnb    @@Za_Loop

@@Process_Zb:
  or       ax, ax
  jz     @@EndProc
  mov      [MixCount], ax      ; init MixCount value
  mov      [NrB2Mix], ax       ; init parameter of the _MIX procedure
  call     MONO_MIX_8BIT_I     ; mix Mixbuflen times

@@EndProc:
  ret
ENDP _SBMONO_MIXER_I


; %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
; %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
; %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
; %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

; %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
; This is the stereo mixer: 8 bit, without interpolation
; this one has no Vol0 optimisations yet
; %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

PROC STEREO_MIX_8BIT NEAR
  push    bp
  mov     al, [BYTE ModInfo]
  mov     [ChnCnt], al         ; nr of channels the second part has to mix

; init segment regs to point to the 2 buffers: ------------------------------
  lfs     di, [TMixBuffer]     ; fs:0 = TMixBuffer^[0]  N-O-T !!!! ofs != 0(?)
  lgs     di, [MixBuffer]      ; gs:0 = MixBuffer^[0]   N-O-T !!!! ofs != 0(?)
  mov     si, OFFSET MixInfo   ; Init si w/ the address of the variable
  sub     si, [MixInfo_Size]

; ***************************************************************************
; First Clean up the temporary DWORD buffer:
; ***************************************************************************

  les     di, [TMixBuffer]
  xor     di, di               ; Hum... BUT NEEDED!
  xor     eax, eax
  mov     cx, [nrB2Mix]        ; NrB2Mix times
  shl     cx, 1
  cld                          ; forward; inc di
  rep     stosd                ; fill es:di with zeros

; ***************************************************************************
; Now mix all the channels together:
; ***************************************************************************

ST_MIX_CHANNELS:
  cmp     [ChnCnt], 0
  jz      ST_FINISH_MIXING
  dec     [ChnCnt]             ; decrement loop counter
  add     si, [MixInfo_Size]
  cmp     [BYTE ds:si], 0      ; Should I mix this channel?
  jz      ST_MIX_CHANNELS      ; no, skip channel

; Calculate some base addresses & var's: ------------------------------------

  mov     ax, [Amplify]        ; precalc amplify     (ampli = max 127)
  mul     [BYTE ds:si+2]       ; mul by volume left  (vol   = max 64)
  mov     [TmpAmpL], ax
  mov     ax, [Amplify]        ; precalc amplify     (ampli = max 127)
  mul     [BYTE ds:si+3]       ; mul by volume right (vol   = max 64)
  mov     [TmpAmpR], ax

  mov     ax, [NrB2Mix]        ; init loop counters
  mov     [MixLoopCnt], ax
  mov     es , [ds:si+11]      ; es  = seg(samples[Instr]^)
  mov     di , [ds:si+13]      ; di  = Ofs(samples[Instr]^)
  mov     edx, [ds:si+19]      ; edx = RealIndex
  xor     ecx, ecx             ; cx  = TMixIndex
  xor     eax, eax             ; only do it once!

; start of Vol0 optimisation code
  cmp     [BYTE ds:si+1], MIN_VOL ; if volume < MIN_VOL -=> don't mix!
;  jae   @@_StartMixing
  jmp   @@_StartMixing         ; ** !!! NO VOLUME ZERO OPTIMISATION YET !!! **
  mov     ax, [MixLoopCnt]     ; nr of loops
  mul     [DWORD ds:si+15]     ; multiply w/ IncEr
  add     eax, [ds:si+19]      ; eax:edx = total IncEr; adjust w/ RealIndex
  adc     edx, 0               ; add carrier
  mov     bx, [ds:si+9]        ; bx = length
  shl     ebx, 16              ; ebx = length << 16
  or      edx, edx             ; edx = 0 ?
  jnz   @@_GoForAdjust
  cmp     eax, ebx
  jae   @@_GoForAdjust
  mov     [ds:si+19], eax      ; adjust RealIndex value
  jmp     ST_MIX_CHANNELS
@@_GoForAdjust:
  cmp     [BYTE ds:si+4], 0    ; [ds:di+4] = RepeatSample
  jnz   @@_RepLenDivide
  mov     [BYTE ds:si], 0      ; OnMix = False, stop mixing (no repeat)
  jmp     ST_MIX_CHANNELS
@@_RepLenDivide:
  mov     bx, [ds:si+5]        ; ebx = Repeat Offset
  shl     ebx, 16              ;                      << 16
  sub     eax, ebx             ; substract Repeat Offset
  sbb     edx, 0               ; complete substraction
  mov     bx, [ds:si+7]        ; ebx = repeat Length
  shl     ebx, 16              ;                      << 16
  div     ebx                  ; divide by repeat length;
  mov     bx, [ds:si+5]        ; ebx = Repeat Offset
  shl     ebx, 16              ;                      << 16
  add     edx, ebx             ; add Repeat Offset
  mov     [ds:si+19], edx      ; put rest into RealIndex
  xor     cx, cx               ; set temp buffer index to zero
  jmp     ST_MIX_CHANNELS
; end of Vol0 opti code.

@@_StartMixing:
  mov     ebp, edx             ; ebp = RealIndex

@@_MixLoopStart:               ; Start of Loop
  mov     ebx, ebp
  add     ebp, [ds:si+15]      ; increment sample data index w/ freq inc'er
  shr     ebx, 16
  cmp     bx, [ds:si+9]        ; check if I have to re-mix
  jae   @@_Check4Repeat

  mov     al, [es:di+bx]       ; get bytes from sample data
  mov     bh, al
  cbw                          ; make a 16 bit integer

  imul    [TmpAmpL]            ; adjust w/ amplify & volume
  shl     edx, 16              ; convert ax:dx to edx
  mov     dx,  ax              ;

  add     [fs:ecx], edx
  add     ecx, 4

  mov     al, bh
  cbw                          ; make a 16 bit integer
  imul    [TmpAmpR]            ; adjust w/ amplify & volume
  shl     edx, 16              ; convert ax:dx to edx
  mov     dx,  ax              ;

  add     [fs:ecx], edx
  add     ecx, 4

  dec     [MixLoopCnt]
  jnz   @@_MixLoopStart
  mov     [ds:si+19], ebp
  jmp     ST_MIX_CHANNELS      ; don't change the rest of the buffer

@@_Check4Repeat:
  cmp     [BYTE ds:si+4], 0    ; [ds:di+4] = RepeatSample
  jz    @@_NoSampleRepeat      ; Don't repeat sample
  push    bp
  mov     bp, [ds:si+5]        ; edx = RepeatOffset
  shl     ebp, 16              ;                     * $10000
  pop     bp
  jmp   @@_MixLoopStart        ; continue Mixing
@@_NoSampleRepeat:
  mov     [BYTE ds:si], 0      ; OnMix = False
  jmp     ST_MIX_CHANNELS

; ***************************************************************************

ST_FINISH_MIXING:
  xor     bx, bx
  mov     cx, [NrB2Mix]        ; NrB2Mix != 0 !!!
  shl     cx, 1
  mov     di, [MixIndex]
  add     [MixIndex], cx       ; YES! Needed, do NOT remove!

@@_3_EndLoop:
  mov     eax, [fs:bx]

  cmp     eax, [MinA]          ; check boundaries here!
  jnl   @@_CheckMaxA           ; MinA:=LongInt(-128*64*16*ModInfo.NrChannels)
  mov     eax, [MinA]          ; MaxA:=LongInt( 127*64*16*ModInfo.NrChannels)
  jmp   @@_EndAmpCheck
@@_CheckMaxA:
  cmp     eax, [MaxA]
  jng   @@_EndAmpCheck
  mov     eax, [MaxA]
@@_EndAmpCheck:

  sar     eax, 10              ; adjust: div by max. volume (64) & amp (16)
  idiv    [BYTE ModInfo]       ; div by chn
  xor     al, 80h              ; SoundBlaster needs unsigned data!
  mov     [gs:di], al          ; al = Mixed byte, push to buffer
  inc     di
  add     bx, 4                ; increment Index of temp buffer
  dec     cx
  jnz   @@_3_EndLoop

@@FINISHED_MIXING:
  pop     bp
  ret
ENDP STEREO_MIX_8BIT


; %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
; This is the stereo mixer: 8 bit, with interpolation
; %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


PROC STEREO_MIX_8BIT_I NEAR
  push    bp
  mov     al, [BYTE ModInfo]
  mov     [ChnCnt], al         ; nr of channels the second part has to mix

; init segment regs to point to the 2 buffers: ------------------------------
  lfs     di, [TMixBuffer]     ; fs:0 = TMixBuffer^[0] N-O-T !!!! ofs != 0(?)
  lgs     di, [MixBuffer]      ; gs:0 = MixBuffer^[0]  N-O-T !!!! ofs != 0(?)
  mov     si, OFFSET MixInfo   ; Init si w/ the address of the variable
  sub     si, [MixInfo_Size]

; ***************************************************************************
; First Clean up the temporary DWORD buffer:
; ***************************************************************************

  les     di, [TMixBuffer]
  xor     di, di               ; Hum... BUT NEEDED!
  xor     eax, eax
  mov     cx, [nrB2Mix]        ; NrB2Mix times
  shl     cx, 1
  cld                          ; forward; inc di
  rep     stosd                ; fill es:di with zeros

; ***************************************************************************
; Now mix all the channels together:
; ***************************************************************************

ST_MIX_CHANNELS_I:
  cmp     [ChnCnt], 0
  jz      ST_FINISH_MIXING_I
  dec     [ChnCnt]             ; decrement loop counter
  add     si, [MixInfo_Size]
  cmp     [BYTE ds:si], 0      ; Should I mix this channel?
  jz      ST_MIX_CHANNELS_I    ; no, skip channel

; Calculate some base addresses & var's: ------------------------------------

  mov     ax, [Amplify]        ; precalc amplify (ampli = max 127)
  mul     [BYTE ds:si+2]       ; mul by volume L (vol   = max 64)
  mov     [TmpAmpL], ax        ; TmpAmpL = amplify * LeftVol

  mov     ax, [Amplify]        ; precalc amplify (ampli = max 127)
  mul     [BYTE ds:si+3]       ; mul by volume R (vol   = max 64)
  mov     [TmpAmpR], ax        ; TmpAmpR = Amplify * RightVol


  mov     ax, [NrB2Mix]        ; init loop counters
  mov     [MixLoopCnt], ax
  mov     es , [ds:si+11]      ; es  = seg(samples[Instr]^)
  mov     di , [ds:si+13]      ; di  = Ofs(samples[Instr]^)
  mov     edx, [ds:si+19]      ; edx = RealIndex
  xor     ecx, ecx             ; cx  = TMixIndex
  xor     eax, eax             ; only do it once!

; start of Vol0 optimisation code
  cmp     [BYTE ds:si+1], MIN_VOL ; if volume < MIN_VOL -=> don't mix!
;  jae   @@_StartMixing
  jmp   @@_StartMixing         ; ** !!! NO VOLUME ZERO OPTIMISATION YET !!! **
  mov     ax, [MixLoopCnt]     ; nr of loops
  mul     [DWORD ds:si+15]     ; multiply w/ IncEr
  add     eax, [ds:si+19]      ; eax:edx = total IncEr; adjust w/ RealIndex
  adc     edx, 0               ; add carrier
  mov     bx, [ds:si+9]        ; bx = length
  shl     ebx, 16              ; ebx = length << 16
  or      edx, edx             ; edx = 0 ?
  jnz   @@_GoForAdjust
  cmp     eax, ebx
  jae   @@_GoForAdjust
  mov     [ds:si+19], eax      ; adjust RealIndex value
  jmp     ST_MIX_CHANNELS_I
@@_GoForAdjust:
  cmp     [BYTE ds:si+4], 0    ; [ds:di+4] = RepeatSample
  jnz   @@_RepLenDivide
  mov     [BYTE ds:si], 0      ; OnMix = False, stop mixing (no repeat)
  jmp     ST_MIX_CHANNELS_I
@@_RepLenDivide:
  mov     bx, [ds:si+5]        ; ebx = Repeat Offset
  shl     ebx, 16              ;                      << 16
  sub     eax, ebx             ; substract Repeat Offset
  sbb     edx, 0               ; complete substraction
  mov     bx, [ds:si+7]        ; ebx = repeat Length
  shl     ebx, 16              ;                      << 16
  div     ebx                  ; divide by repeat length;
  mov     bx, [ds:si+5]        ; ebx = Repeat Offset
  shl     ebx, 16              ;                      << 16
  add     edx, ebx             ; add Repeat Offset
  mov     [ds:si+19], edx      ; put rest into RealIndex
  xor     cx, cx               ; set temp buffer index to zero
  jmp     ST_MIX_CHANNELS_I
; end of Vol0 opti code.

@@_StartMixing:
  mov     ebp, edx             ; ebp = RealIndex

@@_MixLoopStart:               ; Start of Loop
  mov     ebx, ebp
  add     ebp, [ds:si+15]      ; increment sample data index w/ freq inc'er
  shr     bx, 1                ; get rid of a bit ;-)                  2
  mov     [frac], bx           ;                                       1
  shr     ebx, 16
  cmp     bx, [ds:si+9]        ; check if I have to re-mix
  jae   @@_Check4Repeat

  mov     ax, [es:di+bx]       ; get bytes from sample data            1
  mov     bh, ah               ; save second byte                      1
  cbw                          ; make a 16 bit integer                 3
  mov     [y1], ax             ; save it for later                     1
  mov     dx, ax               ; back it up                            1
  mov     al, bh
  cbw                          ; make a 16 bit integer                 3,1

  sub     ax, dx               ; calc delta: ax == Yd == Y2-Y1; -255<Yd<+255
  imul    [frac]               ; A=frac*Yd                             26
  shl     edx, 16              ;                                       2
  mov     dx, ax               ; dx:ax -> edx                          1
  mov     ax, [y1]             ; al == y1                              1
  shl     eax, 17              ;                                       2
  sar     eax, 2               ;                                       2
  add     eax, edx             ;                                       1, 2
  sar     eax, 15              ; total == 52 clocks for interpol. routine
  mov     [y1], ax             ; y1 = interpolated sample

  imul    [TmpAmpL]            ; adjust w/ amplify & volume
  shl     edx, 16              ; convert ax:dx to edx
  mov     dx,  ax              ;
  add     [fs:ecx], edx        ; mix with previous channels in temp dword buf
  add     ecx, 4               ; increment temp dword buf index
  mov     ax, [y1]
  imul    [TmpAmpR]            ; adjust w/ amplify & volume
  shl     edx, 16              ; convert ax:dx to edx
  mov     dx,  ax              ;
  add     [fs:ecx], edx        ; mix with previous channels in temp dword buf
  add     ecx, 4               ; increment temp dword buf index

  dec     [MixLoopCnt]
  jnz   @@_MixLoopStart
  mov     [ds:si+19], ebp
  jmp     ST_MIX_CHANNELS_I    ; don't change the rest of the buffer

@@_Check4Repeat:
  cmp     [BYTE ds:si+4], 0    ; [ds:di+4] = RepeatSample
  jz    @@_NoSampleRepeat      ; Don't repeat sample
  push    bp
  mov     bp, [ds:si+5]        ; edx = RepeatOffset
  shl     ebp, 16              ;                     * $10000
  pop     bp
  jmp   @@_MixLoopStart        ; continue Mixing
@@_NoSampleRepeat:
  mov     [BYTE ds:si], 0      ; OnMix = False
  jmp     ST_MIX_CHANNELS_I

; ***************************************************************************

ST_FINISH_MIXING_I:
  xor     bx, bx
  mov     cx, [NrB2Mix]        ; NrB2Mix != 0 !!!
  shl     cx, 1
  mov     di, [MixIndex]
  add     [MixIndex], cx       ; YES! Needed, do NOT remove!

@@_3_EndLoop:
  mov     eax, [fs:bx]

  cmp     eax, [MinA]          ; check boundaries here!
  jnl   @@_CheckMaxA           ; MinA:=LongInt(-128*64*16*ModInfo.NrChannels)
  mov     eax, [MinA]          ; MaxA:=LongInt( 127*64*16*ModInfo.NrChannels)
  jmp   @@_EndAmpCheck
@@_CheckMaxA:
  cmp     eax, [MaxA]
  jng   @@_EndAmpCheck
  mov     eax, [MaxA]
@@_EndAmpCheck:

  sar     eax, 10              ; adjust: div by max. volume (64) & amp (16)
  idiv    [BYTE ModInfo]       ; div by nr of channels
  xor     al, 80h              ; SoundBlaster needs unsigned data!
  mov     [gs:di], al          ; al = Mixed byte, push to buffer
  inc     di
  add     bx, 4                ; increment Index of temp dword buffer
  dec     cx
  jnz   @@_3_EndLoop

@@FINISHED_MIXING:
  pop     bp
  ret
ENDP STEREO_MIX_8BIT_I


; %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
; now comes the PROC that controls the stereo mixer:
; %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


PROC _SBSTEREO_MIXER NEAR
  mov      [MixIndex], 0       ; initialise mixing counter
  mov      ax, [CallBpm]
  mov      bx, [MixCount]
  mov      cx, [MixBuflen]
  shr      cx, 1
  sub      ax, bx
  mov      [X], ax             ; X = CallBpm - MixCount
  cmp      ax, cx              ; X <= MixBuflen / 2 ?
  jna    @@X_EQUAL_OR_LOWER

@@X_BIGGER:
  add      bx, cx
  mov      [MixCount], bx      ; MixCount = MixCount + MixBuflen
  mov      [NrB2Mix], cx       ; init parameter of the STEREO_MIX_8BIT proc.
  call     STEREO_MIX_8BIT     ; mix Mixbuflen times
  ret                          ; Ok, exit

@@X_EQUAL_OR_LOWER:
  sub      cx, ax
  mov      [X], cx             ; X = MixBufLen - X
  mov      [NrB2Mix], ax       ; init parameter of the STEREO_MIX_8BIT proc.
  call     STEREO_MIX_8BIT     ; mix X times
  mov      [MixCount], 0       ; reset mix counter
  call     _UPDATEBPM          ; update bpm
  mov      ax, [X]
  cmp      ax, [Callbpm]
  jb     @@Process_Zb

@@Za_Loop:
  mov      cx, [CallBpm]
  mov      [NrB2Mix], cx       ; init parameter of the STEREO_MIX_8BIT proc.
  sub      [X], cx
  call     STEREO_MIX_8BIT     ; mix CallBpm times
  call     _UPDATEBPM          ; update bpm
  mov      ax, [X]
  cmp      ax, [Callbpm]
  jnb    @@Za_Loop

@@Process_Zb:
  or       ax, ax
  jz     @@EndProc
  mov      [MixCount], ax      ; init MixCount value
  mov      [NrB2Mix], ax       ; init parameter of the STEREO_MIX_8BIT proc.
  call     STEREO_MIX_8BIT     ; mix Mixbuflen times

@@EndProc:
  ret
ENDP _SBSTEREO_MIXER


; %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
; now comes the PROC that controls the stereo interpolative mixer:
; %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


PROC _SBSTEREO_MIXER_I NEAR
  mov      [MixIndex], 0       ; initialise mixing counter
  mov      ax, [CallBpm]
  mov      bx, [MixCount]
  mov      cx, [MixBuflen]
  shr      cx, 1
  sub      ax, bx
  mov      [X], ax             ; X = CallBpm - MixCount
  cmp      ax, cx              ; X <= MixBuflen ?
  jna    @@X_EQUAL_OR_LOWER

@@X_BIGGER:
  add      bx, cx
  mov      [MixCount], bx      ; MixCount = MixCount + MixBuflen
  mov      [NrB2Mix], cx       ; init parameter of the STEREO_MIX_8BIT proc.
  call     STEREO_MIX_8BIT_I   ; mix Mixbuflen times
  ret                          ; Ok, exit

@@X_EQUAL_OR_LOWER:
  sub      cx, ax
  mov      [X], cx             ; X = MixBufLen - X
  mov      [NrB2Mix], ax       ; init parameter of the STEREO_MIX_8BIT proc.
  call     STEREO_MIX_8BIT_I   ; mix X times
  mov      [MixCount], 0       ; reset mix counter
  call     _UPDATEBPM          ; update bpm
  mov      ax, [X]
  cmp      ax, [Callbpm]
  jb     @@Process_Zb

@@Za_Loop:
  mov      cx, [CallBpm]
  mov      [NrB2Mix], cx       ; init parameter of the STEREO_MIX_8BIT proc.
  sub      [X], cx
  call     STEREO_MIX_8BIT_I   ; mix CallBpm times
  call     _UPDATEBPM          ; update bpm
  mov      ax, [X]
  cmp      ax, [Callbpm]
  jnb    @@Za_Loop

@@Process_Zb:
  or       ax, ax
  jz     @@EndProc
  mov      [MixCount], ax      ; init MixCount value
  mov      [NrB2Mix], ax       ; init parameter of the STEREO_MIX_8BIT proc.
  call     STEREO_MIX_8BIT_I   ; mix Mixbuflen times

@@EndProc:
  ret
ENDP _SBSTEREO_MIXER_I

; %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
; This procedure chooses the right mixer
; %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


PROC CHOOSEMIXER NEAR
  cmp      [StereoReplay], 0
  jnz    @@StereoMixer
  cmp      [Interpolation], 0
  jnz    @@HWFilter_Mono
@@NoFilter_Mono:
  call     MONO_MIX_8BIT
  ret
@@HWFilter_Mono:
  cmp      [Interpolation], 1
  jne    @@LIFilter_Mono
  call     MONO_MIX_8BIT
  ret
@@LIFilter_Mono:
  cmp      [Interpolation], 2
  jne    @@CIFilter_Mono
  call     MONO_MIX_8BIT_I
  ret
@@CIFilter_Mono:
  cmp      [Interpolation], 3     ; safety check
  jne    @@NoFilter_Mono
;  call     MONO_MIX_8BIT_CI     ; temp. disabled
  call     MONO_MIX_8BIT_I
  ret

@@StereoMixer:
  cmp      [Interpolation], 0
  jnz    @@HWFilter_Stereo
@@NoFilter_Stereo:
  call     STEREO_MIX_8BIT
  ret
@@HWFilter_Stereo:
  cmp      [Interpolation], 1
  jne    @@LIFilter_Stereo
  call     STEREO_MIX_8BIT
  ret
@@LIFilter_Stereo:
  cmp      [Interpolation], 2
  jne    @@CIFilter_Stereo
  call     STEREO_MIX_8BIT_I
  ret
@@CIFilter_Stereo:
  cmp      [Interpolation], 3     ; safety check
  jne    @@NoFilter_Stereo
;  call     STEREO_MIX_8BIT_CI     ; temp. disabled
  call     STEREO_MIX_8BIT_I
  ret
ENDP CHOOSEMIXER

; %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
; now comes the PROC that controls the different mixers:
; %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


PROC _SB_MIXER NEAR
  mov      [MixIndex], 0       ; initialise mixing counter
  mov      ax, [CallBpm]
  mov      bx, [MixCount]
  mov      cx, [MixBuflen]
  cmp      [StereoReplay], 0
  jz     @@MonoMixer
  shr      cx, 1               ; stereo? / 2 'coz buf 'll fill twice as fast
@@MonoMixer:
  sub      ax, bx
  mov      [X], ax             ; X = CallBpm - MixCount
  cmp      ax, cx              ; X <= MixBuflen ?
  jna    @@X_EQUAL_OR_LOWER

@@X_BIGGER:
  add      bx, cx
  mov      [MixCount], bx      ; MixCount = MixCount + MixBuflen
  mov      [NrB2Mix], cx       ; init parameter of the mixing proc.
  call     CHOOSEMIXER         ; mix Mixbuflen times
  ret                          ; Ok, exit

@@X_EQUAL_OR_LOWER:
  sub      cx, ax
  mov      [X], cx             ; X = MixBufLen - X
  mov      [NrB2Mix], ax       ; init parameter of the mixing proc.
  call     CHOOSEMIXER         ; mix X times
  mov      [MixCount], 0       ; reset mix counter
  call     _UPDATEBPM          ; update bpm
  mov      ax, [X]
  cmp      ax, [Callbpm]
  jb     @@Process_Zb

@@Za_Loop:
  mov      cx, [CallBpm]
  mov      [NrB2Mix], cx       ; init parameter of the mixing proc.
  sub      [X], cx
  call     CHOOSEMIXER         ; mix CallBpm times
  call     _UPDATEBPM          ; update bpm
  mov      ax, [X]
  cmp      ax, [Callbpm]
  jnb    @@Za_Loop

@@Process_Zb:
  or       ax, ax
  jz     @@EndProc
  mov      [MixCount], ax      ; init MixCount value
  mov      [NrB2Mix], ax       ; init parameter of the mixing proc.
  call     CHOOSEMIXER         ; mix Mixbuflen times

@@EndProc:
  ret
ENDP _SB_MIXER











ENDS CODE

END


