        IDEAL
        P486

SEGMENT DATA word public

EXTRN       Res16, Par16a, Par16b: WORD
EXTRN       Res32, Par32a, Par32b: DWORD
EXTRN       Res64, Par64a, par64b: QWORD
EXTRN       ResFloat , ParFloata , ParFloatb : DWORD
EXTRN       ResDouble, ParDoublea, ParDoubleb: QWORD

TmpAmp      dw      ?
frac        dw      ?
p0          dw      ?
p1          dw      ?
p2          dw      ?
p3          dw      ?
tmp16b      dw      ?
tmp16       dw      ?
tmp32       dd      ?

ENDS DATA


SEGMENT CODE byte public


ASSUME  cs:CODE, ds:DATA


PUBLIC  TESTFPU


PROC PASCAL_SAR

ENDP PASCAL_SAR



; %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
; %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
; WARNING - FULL TYPECASTING IS NECESSARY EVEN IF TASM KNOWS THE SIZE OF
; THE VARIABLE!!!!


PROC TESTFPU NEAR
;  FINIT
;  FILD     [DWORD PTR Par32a]   ; load 32b integer
;  FIDIV    [DWORD PTR Par32b]   ; divide by Par32b
;  FST      [QWORD PTR Res64]    ; save result
;  FIST     [DWORD PTR Res32]    ; save result

  FINIT

@@_MixLoopStart:               ; Start of Loop
;  mov     ebx, ebp
;  add     ebp, [ds:si+15]      ; increment sample data index w/ freq inc'er

;  mov     [TmpAmp], 64*16
  mov     [TmpAmp], 1
;  mov     bx, 21845            ; 65536/3
  mov     bx, 32768             ; 65536/2

  shr     bx, 1                ; get rid of a bit, or it would be a signed int
  mov     [frac], bx           ; and we don't want that!
  FILD    [WORD PTR frac]
  mov     [tmp32], 32768       ;
  FIDIV   [DWORD PTR tmp32]    ; divide S(0) by 32768: S(0) = Frac/65536 = f
;  FST     [DWORD PTR ResFloat]
;  FST     [QWORD PTR ResDouble]


;  shr     ebx, 16
;  cmp     bx, [ds:si+9]        ; check if I have to re-mix
;  jae   @@_Check4Repeat

@@GetFirstTwoSamples:
;  mov     ax, [es:di+bx]
  mov     al, -7               ; p0 = -7
  mov     ah, -5               ; p1 = -5
  movsx   dx, ah
  cbw
  mov     [p0], ax
  mov     [p1], dx
  add     bx, 2

@@GetNextTwoSamples:
;  mov     ax, [es:di+bx]
  mov     al, 3                ; p2 = 3
  mov     ah, 10               ; p3 = 10

  movsx   cx, ah               ; cx = [p3]
  cbw
  mov     [p2], ax

  sub     dx, ax               ; dx = [p1]
  mov     ax, dx               ; ax = t = p1 - p2
  shl     ax, 1
  add     ax, dx               ; ax = (p1 - p2) * 3
  sub     ax, [p0]
  add     ax, cx               ; cx = [p3]

;  imul    [frac]
;  shl     edx, 16              ;
;  mov     dx, ax               ; dx:ax -> edx

  mov     [tmp16b], ax
  FILD    [WORD PTR tmp16b]    ; S(1) = f, S(0) = a
  FMUL    ST(0), ST(1)         ; S(0) = ax * f

  mov     bx, [p2]             ; bx = [p2]
  shl     bx, 1                ; bx = [p2]
  add     bx, [p0]
  mov     ax, [p1]
  shl     ax, 2
  add     ax, [p1]
  add     ax, cx
  sar     ax, 1                ; WARNING: (- 1) SAR x = - 1 !!!
  sub     bx, ax               ; ax = b = p2 shl 1 + p0  - (p1 * 5 + p3) div 2
;  mov     ax, bx

  mov     [tmp16b], bx
  FIADD   [WORD PTR tmp16b]    ; S(0) = S(0) + ax; S(1) = f
  FMUL    ST(0), ST(1)         ; S(0) = S(0) * f


;  shl     eax, 16
;  add     eax, edx
;  sar     eax, 11              ; add a bit thas was removed from [frac]

;  imul    [frac]               ; pascal line 2
;  shl     edx, 16              ;
;  mov     dx, ax               ; dx:ax -> edx

  mov     ax, [p2]
  sub     ax, [p0]             ; ax = c = (p2 - p0) //div 2

  sar     ax, 1
  mov     [tmp16b], ax
  FIADD   [WORD PTR tmp16b]    ; S(0) = S(0) + ax; S(1) = f
  FMULP   ST(1), ST(0)         ; S(0) = f * S(0)

;  shl     eax, 19
;  add     eax, edx
;  sar     eax, 15              ; add a bit that was removed from [frac]


;  imul    [frac]
;  shl     edx, 16              ;
;  mov     dx, ax               ; dx:ax -> edx

;  mov     ax, [p1]
;  shl     eax, 20
;  add     eax, edx
;  sar     eax, 16

  FIADD   [WORD PTR p1]        ; S(0) = S(0) + [p1]
  FIMUL   [WORD PTR TmpAmp]    ; S(0) = S(0) * TmpAmp
  FST     [DWORD PTR ResFloat]
  FST     [QWORD PTR ResDouble]
  FISTP   [DWORD PTR Res32]    ; Store and pop S(0)

;  imul    [TmpAmp]             ; adjust w/ amplify & volume
;  shl     edx, 16              ; convert ax:dx to edx
;  mov     dx, ax
;  sar     edx, 4               ; WARNING: (- 1) SAR x = - 1 !!!

;  mov     cx, [TMixIndex]

  mov      edx, [DWORD PTR Res32]; store result

;  add     [fs:ecx], edx
;  add     cx, 4

@@_Check4Repeat:

  ret
ENDP TESTFPU


ENDS CODE

END


