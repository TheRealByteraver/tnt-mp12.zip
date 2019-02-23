;%===========================================================================%
;%===========================================================================%
;
; Compile with TASM: "TASM /MU /ZN /OS DMA_MIX?.ASM"
;
; W-A-R-N-I-N-G ! These Routines don't mix channels if their volume is 0!
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
; -----------------------------------------
;
; Instead of dividing each mixed DWORD by the nr of channels in the MOD,
; I should do a shift. This doesn't require an extra instruction; instead of
; doing
;
;      shr     eax, 6        ; adjust by max. volume
;
; I'll have to do (example for 4 channels):
;
;      shr     eax, 6+2      ; adjust by max. volume, divide by nr of channels
;
; In fact, the final code should look like this:
;
;      mov     cl, ûnr_of_chn; outside loop
;      add     cl, 6         ; outside loop
;      shr     eax, cl       ; (This instruction keeps cl intact, so no prob!)
;
; Of course, if the nr of channels is not a power of 2 (e.g. 10, 12, 14...)
; I'll have to round the shift, which will give bad quality. But since I could
; remove a div instruction in the loop, I can insert a Mul instruction for
; amplification. When a proper ampl. is used, the problem stated above is
; solved!
;
;
;%===========================================================================%
;%===========================================================================%



        IDEAL
        P386

MIN_VOL  EQU  4

SEGMENT DATA word public

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
dummy       dw      ?
frac        dw      ?

y1          db      ?
ChnCnt      db      ?
;word_align  db      ?     ; to keep word alignment intact in main prog.

ENDS DATA


SEGMENT CODE byte public

ASSUME  cs:CODE, ds:DATA

PUBLIC  _SBMONO_MIXER, _SBMONO_MIXER_I, _SBSTEREO_MIXER, _SWAPBUFFERS

EXTRN   SBUpdateMultipleStepsEffects: NEAR, SBUpdateNotes: NEAR


; %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
; now comes the mixing proc itself: this is the mono mixer...
; (YES YES YES GUYS! HERE IT IS! ;-))
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

  mov     ax, [Amplify]        ; precalc amplify (ampli = max 64)
  mul     [BYTE ds:si+1]       ; mul by volume   (vol   = max 64)
;  shl     ax, 3                ; get some spare bits ;-)
;  xor     dx, dx
;  xor     bx, bx
;  mov     bl, [ModInfo]
;  div     bx                   ; divide by nr of channels
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
  mov     [frac], bx           ;                                        1
  shr     ebx, 16              ;
  cmp     bx, [ds:si+9]        ; check if I have to re-mix
  jae   @@_Check4Repeat

  mov     al, [es:di+bx]       ; get byte from sample data              1
  mov     [y1], al             ;                                        1
  cbw                          ; extend sign from al to ax == Yd        3
  mov     dx, ax               ;                                        1
  inc     bx                   ;                                        1
  mov     al, [es:di+bx]       ; get byte from sample data              1
  cbw                          ;                                        3
  sub     ax, dx               ; ax == Yd == Y2-Y1                      1
  cwde                         ;                                        3
  xor     ebx, ebx             ;                                        1
  mov     bx, [frac]           ;                                        1
  imul    ebx                  ; A=frac*Yd                              42
  mov     ebx, eax             ;                                        1
  mov     al, [y1]             ; al == y1                               1
  cbw                          ;                                        3
  cwde                         ;                                        3
  shl     eax, 16              ;                                        2
  add     eax, ebx             ;                                        1
  sar     eax, 16              ;  total == 73 clocks                    2

  imul    [TmpAmp]             ; adjust w/ amplify & volume
  shl     edx, 16              ; convert ax:dx to eax
  mov     dx,  ax              ;
;  cmp     edx, [MinA]
  cmp     edx, -128*64*16      ; check boundaries
  jnl   @@_CheckMaxA
  mov     edx, [MinA]
  jmp   @@_EndAmpCheck
@@_CheckMaxA:
;  cmp     edx, [MaxA]
  cmp     edx, +127*64*16      ; check boundaries
  jng   @@_EndAmpCheck
  mov     edx, [MaxA]
@@_EndAmpCheck:
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
;  sar     eax, 13              ; adjust: div by max. volume (64) & amp (16)
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
; now comes the updateBPM proc:
; %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


PROC _UPDATEBPM NEAR
  inc     [Timing]             ; Yes, update bpm (and FX)
  mov     al, [Timing]
  cmp     al, [Timing+2]       ; BpmCount = Speed ?
  jb    @@Else
  mov     ax, [WORD Timing+4]  ; ax = Timing.PatternDelay
  cmp     ax, 0
  jnz    @@Else
  call    SBUpDateNotes        ; Yes, update note info & FX
  inc     [Waitstate]
  mov     [Timing], 0          ; Timing.count = 0
  ret
@@Else:                        ; Update FX only (vibrato, arpeggio, ...):
  call    SBUpDateMultipleStepsEffects
  ret
ENDP _UPDATEBPM


; %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
; now comes the PROC that swaps the two external buffers:
; %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


PROC _SWAPBUFFERS NEAR
  mov      eax, [MixBuffer ]   ; swap mix & play buffers
  mov      ebx, [PlayBuffer]
  mov      [MixBuffer ], ebx
  mov      [PlayBuffer], eax
  ret
ENDP _SWAPBUFFERS


; %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
; now comes the PROC that does all the stuff:
; %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

PROC _SBMONO_MIXER_I NEAR
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
ENDP _SBMONO_MIXER_I


; %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
; %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
; %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
; %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


; %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
; now comes the second mixing proc: the stereo mixer!
; this one has no Vol0 optimisations yet
; %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

PROC STEREO_MIX_8BIT NEAR
  mov     ax, [NrB2Mix]        ; init loop counters
  mov     [MixLoopCnt], ax
  mov     al, [BYTE ModInfo]
  sub     al, 2
  mov     [ChnCnt], al         ; nr of channels the second part has to mix

; init segment regs to point to the 2 buffers: ------------------------------
  lfs     di, [TMixBuffer]     ; fs:0 = TMixBuffer^[0]  N-O-T !!!! ofs != 0 ?!
  lgs     di, [MixBuffer]      ; gs:0 = MixBuffer^[0]

; ***************************************************************************
; First mix the first channel (and only the first):
; ***************************************************************************

ST_MIX_FIRST_CHANNEL:
  xor     ecx, ecx             ; cx  = TMixIndex
  mov     si, OFFSET MixInfo   ; Init si w/ the address of the variable
  and     [BYTE ds:si], 0FFh   ; Should I mix this channel?
  jz    @@_1_SkipMix           ; no, skip 1st channel

; Calculate some base addresses & var's: ------------------------------------

  mov     ax, [Amplify]        ; precalc amplify (ampli = max 64)
  mul     [BYTE ds:si+1]       ; mul by volume   (vol   = max 64)
;  shl     ax, 3                ; get some spare bits ;-)
;  xor     dx, dx
;  xor     bx, bx
;  mov     bl, [ModInfo]
;  div     bx                   ; divide by nr of channels
  mov     [TmpAmp], ax

  mov     es , [ds:si+11]      ; es  = seg(samples[Instr]^)
  mov     di , [ds:si+13]      ; di  = Ofs(samples[Instr]^)
  mov     edx, [ds:si+19]      ; edx = RealIndex
  xor     eax, eax             ; only do it once!

@@_1_MixLoopStart:             ; Start of Loop
  mov     ebx, edx
  add     edx, [ds:si+15]      ; increment sample data index w/ freq inc'er
  shr     ebx, 16
  cmp     bx, [ds:si+9]        ; check if I have to re-mix
  jae   @@_1_Check4Repeat
  mov     al, [es:di+bx]       ; get byte from sample data
  imul    [BYTE ds:si+2]       ; adjust it with LEFT volume of the channel

  push    dx                   ; Amplify section
  imul    [Amplify]
  push    ax
  mov     ax, dx
  shl     eax, 16
  pop     ax
  cmp     eax, [MinA]
  jnl     @@_1_CheckMaxA
  mov     eax, [MinA]
  jmp     @@_1_EndAmpCheck
@@_1_CheckMaxA:
  cmp     eax, [MaxA]
  jng     @@_1_EndAmpCheck
  mov     eax, [MaxA]
@@_1_EndAmpCheck:
  pop     dx

;  cwde                         ; extend sign of ax to eax for addition
  mov     [fs:ecx], eax        ; push word on buffer & clean High Word of it
  add     ecx, 4               ; increment Index of temp buffer (dword)
  mov     al, [es:di+bx]       ; get byte from sample data
  imul    [BYTE ds:si+3]       ; adjust it with RIGHT volume of the channel

  push    dx                   ; Amplify section
  imul    [Amplify]
  push    ax
  mov     ax, dx
  shl     eax, 16
  pop     ax
  cmp     eax, [MinA]
  jnl     @@_1b_CheckMaxA
  mov     eax, [MinA]
  jmp     @@_1b_EndAmpCheck
@@_1b_CheckMaxA:
  cmp     eax, [MaxA]
  jng     @@_1b_EndAmpCheck
  mov     eax, [MaxA]
@@_1b_EndAmpCheck:
  pop     dx

;  cwde                         ; extend sign of ax to eax for addition
  mov     [fs:ecx], eax        ; push word on buffer & clean High Word of it
  add     ecx, 4               ; increment Index of temp buffer (dword)
  dec     [MixLoopCnt]
  jnz   @@_1_MixLoopStart
  mov     [ds:si+19], edx
  jmp     ST_MIX_2ND_2_BUTLAST_CHANNEL

@@_1_Check4Repeat:
  cmp     [BYTE ds:si+4], 0    ; [ds:di+4] = RepeatSample
  jz    @@_1_NoSampleRepeat    ; Don't repeat sample
  push    dx
  mov     dx, [ds:si+5]        ; edx = RepeatOffset
  shl     edx, 16              ;                    * $10000
  pop     dx
  jmp   @@_1_MixLoopStart      ; continue Mixing
@@_1_NoSampleRepeat:
  mov     [BYTE ds:si], 0      ; OnMix = False

@@_1_SkipMix:
;  fill the rest of the (or even the whole) buffer w/ 0's:
  mov     bx, cx
  mov     cx, [MixLoopCnt]
  jcxz    ST_MIX_2ND_2_BUTLAST_CHANNEL
  shl     cx, 1
@@_1_FillZero:
  mov     [DWORD fs:bx], 0
  add     bx, 4
  loop  @@_1_FillZero


; ***************************************************************************
; Now mix the second until the butlast channel:
; ***************************************************************************


ST_MIX_2ND_2_BUTLAST_CHANNEL:
  mov     cx, [NrB2Mix]        ; init loop counters
  mov     [MixLoopCnt], cx
  cmp     [ChnCnt], 0
  jz      ST_MIX_LAST_CHANNEL
  dec     [ChnCnt]             ; decrement loop counter
  add     si, [MixInfo_Size]
  and     [BYTE ds:si], 0FFh   ; Should I mix this channel?
  jz      ST_MIX_2ND_2_BUTLAST_CHANNEL ; no, skip channel

; Calculate some base addresses & var's: ------------------------------------

  mov     ax, [Amplify]        ; precalc amplify (ampli = max 64)
  mul     [BYTE ds:si+1]       ; mul by volume   (vol   = max 64)
;  shl     ax, 3                ; get some spare bits ;-)
;  xor     dx, dx
;  xor     bx, bx
;  mov     bl, [ModInfo]
;  div     bx                   ; divide by nr of channels
  mov     [TmpAmp], ax

  mov     es , [ds:si+11]      ; es  = seg(samples[Instr]^)
  mov     di , [ds:si+13]      ; di  = Ofs(samples[Instr]^)
  mov     edx, [ds:si+19]      ; edx = RealIndex
  xor     ecx, ecx             ; cx  = TMixIndex
  xor     eax, eax             ; only do it once!

@@_2_MixLoopStart:             ; Start of Loop
  mov     ebx, edx
  add     edx, [ds:si+15]      ; increment sample data index w/ freq inc'er
  shr     ebx, 16
  cmp     bx, [ds:si+9]        ; check if I have to re-mix
  jae   @@_2_Check4Repeat
  mov     al, [es:di+bx]       ; get byte from sample data
  imul    [BYTE ds:si+2]       ; adjust it with LEFT volume of the channel

  push    dx                   ; Amplify section
  imul    [Amplify]
  push    ax
  mov     ax, dx
  shl     eax, 16
  pop     ax
  cmp     eax, [MinA]
  jnl     @@_2_CheckMaxA
  mov     eax, [MinA]
  jmp     @@_2_EndAmpCheck
@@_2_CheckMaxA:
  cmp     eax, [MaxA]
  jng     @@_2_EndAmpCheck
  mov     eax, [MaxA]
@@_2_EndAmpCheck:
  pop     dx

;  cwde                         ; extend sign of ax to eax for addition
  add     [fs:ecx], eax
  add     ecx, 4
  mov     al, [es:di+bx]       ; get byte from sample data
  imul    [BYTE ds:si+3]       ; adjust it with RIGHT volume of the channel

  push    dx                   ; Amplify section
  imul    [Amplify]
  push    ax
  mov     ax, dx
  shl     eax, 16
  pop     ax
  cmp     eax, [MinA]
  jnl     @@_2bCheckMaxA
  mov     eax, [MinA]
  jmp     @@_2bEndAmpCheck
@@_2bCheckMaxA:
  cmp     eax, [MaxA]
  jng     @@_2bEndAmpCheck
  mov     eax, [MaxA]
@@_2bEndAmpCheck:
  pop     dx

;  cwde                         ; extend sign of ax to eax for addition
  add     [fs:ecx], eax        ; push word on buffer & clean High Word of it
  add     ecx, 4               ; increment Index of temp buffer (dword)
  dec     [MixLoopCnt]
  jnz   @@_2_MixLoopStart
  mov     [ds:si+19], edx
  jmp     ST_MIX_2ND_2_BUTLAST_CHANNEL ;  don't change the rest of the buffer

@@_2_Check4Repeat:
  cmp     [BYTE ds:si+4], 0    ; [ds:di+4] = RepeatSample
  jz    @@_2_NoSampleRepeat    ; Don't repeat sample
  push    dx
  mov     dx, [ds:si+5]        ; edx = RepeatOffset
  shl     edx, 16              ;                     * $10000
  pop     dx
  jmp   @@_2_MixLoopStart      ; continue Mixing
@@_2_NoSampleRepeat:
  mov     [BYTE ds:si], 0      ; OnMix = False
  jmp     ST_MIX_2ND_2_BUTLAST_CHANNEL

; ***************************************************************************

ST_MIX_LAST_CHANNEL:
  add     si, [MixInfo_Size]
  mov     ax, [NrB2Mix]        ; init loop counters
  mov     [MixLoopCnt], ax
  xor     ecx, ecx             ; cx  = TMixIndex

  mov     al, [BYTE ModInfo]
  mov     [ChnCnt], al         ; nr of channels I have to divide by
  and     [BYTE ds:si], 0FFh   ; Should I mix this channel?
  jz    @@_3_SkipMix           ; no, skip 1st channel

; Calculate some base addresses & var's: ------------------------------------

  mov     ax, [Amplify]        ; precalc amplify (ampli = max 64)
  mul     [BYTE ds:si+1]       ; mul by volume   (vol   = max 64)
;  shl     ax, 3                ; get some spare bits ;-)
;  xor     dx, dx
;  xor     bx, bx
;  mov     bl, [ModInfo]
;  div     bx                   ; divide by nr of channels
  mov     [TmpAmp], ax

  mov     es , [ds:si+11]      ; es  = seg(samples[Instr]^)
  mov     di , [ds:si+13]      ; di  = Ofs(samples[Instr]^)
  mov     edx, [ds:si+19]      ; edx = RealIndex
  xor     eax, eax             ; only do it once!

@@_3_MixLoopStart:             ; Start of Loop
  mov     ebx, edx
  add     edx, [ds:si+15]      ; increment sample data index w/ freq inc'er
  shr     ebx, 16
  cmp     bx, [ds:si+9]        ; check if I have to re-mix (dx = Smp. Length)
  jae   @@_3_Check4Repeat
  mov     al, [es:di+bx]       ; get byte from sample data
  push    ax                   ; save it for later!
  imul    [BYTE ds:si+2]       ; adjust it with LEFT volume of the channel

  push    dx                   ; Amplify section
  imul    [Amplify]
  push    ax
  mov     ax, dx
  shl     eax, 16
  pop     ax
  cmp     eax, [MinA]
  jnl     @@_3_CheckMaxA
  mov     eax, [MinA]
  jmp     @@_3_EndAmpCheck
@@_3_CheckMaxA:
  cmp     eax, [MaxA]
  jng     @@_3_EndAmpCheck
  mov     eax, [MaxA]
@@_3_EndAmpCheck:
  pop     dx

;  cwde                         ; extend sign of ax to eax for addition
  add     eax, [fs:ecx]        ; add data from temp mix buffer
  add     ecx, 4               ; increment Index of temp buffer (dword)
;  sar     eax, 6               ; adjust: div by max. volume (64)
  sar     eax, 10              ; adjust: div by max. volume (64)
  idiv    [ChnCnt]             ; should be a shift, but... l8er!
  xor     al, 80h              ; SoundBlaster needs unsigned data!
  mov     bx, [MixIndex]
  mov     [gs:bx], al          ; al = Mixed byte, push to buffer
  inc     [MixIndex]
  pop     ax                   ; get byte from sample data
  imul    [BYTE ds:si+3]       ; adjust it with RIGHT volume of the channel

  push    dx                   ; Amplify section
  imul    [Amplify]
  push    ax
  mov     ax, dx
  shl     eax, 16
  pop     ax
  cmp     eax, [MinA]
  jnl     @@_3bCheckMaxA
  mov     eax, [MinA]
  jmp     @@_3bEndAmpCheck
@@_3bCheckMaxA:
  cmp     eax, [MaxA]
  jng     @@_3bEndAmpCheck
  mov     eax, [MaxA]
@@_3bEndAmpCheck:
  pop     dx

;  cwde                         ; extend sign of ax to eax for addition
  add     eax, [fs:ecx]        ; add data from temp mix buffer
  add     ecx, 4               ; increment Index of temp buffer (dword)
;  sar     eax, 6               ; adjust: div by max. volume (64)
  sar     eax, 10              ; adjust: div by max. volume (64)
  idiv    [ChnCnt]             ; should be a shift, but... l8er!
  xor     al, 80h              ; SoundBlaster needs unsigned data!
  mov     bx, [MixIndex]
  mov     [gs:bx], al          ; al = Mixed byte, push to buffer
  inc     [MixIndex]
  dec     [MixLoopCnt]
  jnz   @@_3_MixLoopStart
  mov     [ds:si+19], edx
  ret                          ; this is the same as jumping to the exit ;-)

@@_3_Check4Repeat:
  cmp     [BYTE ds:si+4], 0    ; [ds:si+4] = RepeatSample
  jz    @@_3_NoSampleRepeat    ; Don't repeat sample
  push    dx
  mov     dx, [ds:si+5]        ; edx = RepeatOffset
  shl     edx, 16              ;                    * $10000
  pop     dx
  jmp   @@_3_MixLoopStart      ; continue Mixing

@@_3_NoSampleRepeat:
  mov     [BYTE ds:si], 0      ; OnMix = False

@@_3_SkipMix:
  mov     bx, cx
  mov     cx, [MixLoopCnt]
  jcxz  @@FINISHED_MIXING
  mov     di, [MixIndex]
  shl     cx, 1
  add     [MixIndex], cx       ; YES! Needed, do NOT remove!
@@_3_EndLoop:
  mov     eax, [fs:bx]
;  sar     eax, 6               ; adjust: div by max. volume (64)
  sar     eax, 10              ; adjust: div by max. volume (64)
  idiv    [ChnCnt]             ; divide by nr of channels
  xor     al, 080h             ; SoundBlaster needs unsigned data!

  mov     [gs:di], al          ; al = Mixed byte, push to buffer
  inc     di
  add     bx, 4                ; increment Index of temp buffer
  loop  @@_3_EndLoop

@@FINISHED_MIXING:
  ret
ENDP STEREO_MIX_8BIT


; %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
; now comes the PROC that swaps the two external buffers:
; %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


PROC _SBSTEREO_MIXER NEAR
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

ENDS CODE

END


