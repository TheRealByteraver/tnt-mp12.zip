Uses NewCrt, Mode_VGA;

Type
  ByteBuffer = array[0..$FFF0] of ShortInt;
  TByteBuffer = ^ByteBuffer;

Const
  Zoom = 8;


Procedure DrawWave(Wave: TByteBuffer; BufLen: Word);
Var
  I: Integer;
  x1, y1, x2, y2: word;
Begin
  x1:=0; y1:=200 div 2;
  For I:=0 to bufLen-1 do
  begin
    x2:=I*Zoom;
    y2:=100-Wave^[I];
    Line(x1, y1, x2, y1, White);
    Line(x2, y1, x2, y2, White);
    x1:=x2; y1:=y2;
  end;
End;


Procedure DrawLinear(Wave: TByteBuffer; BufLen: Word);
Var
  I: Integer;
  x1, y1, x2, y2: word;
Begin
  x1:=0; y1:=200 div 2;
  For I:=0 to bufLen-1 do
  begin
    x2:=I*Zoom;
    y2:=100-Wave^[I];
    Line(x1, y1, x2, y2, Brown);
    x1:=x2; y1:=y2;
  end;
End;


Procedure MONO_MIX_8BIT_IC(smp: TByteBuffer; BufLen: Word);
VAR
  chn : Byte;
  p0, p1, p2, p3, a, b, c, t: Integer;
  f2, tmp, intp, fract: LongInt;
  d, TmpAmp: Word;
  f: double;

  I, i2: Integer;
  x1, y1, x2, y2: word;

BEGIN
  x1:=0; y1:=200 div 2;

  For I:=0 to (Buflen-1)*Zoom do
  Begin
{    F:=(I mod Zoom) / Zoom;}
    Fract:=(LongInt(I mod Zoom) shl 16) div Zoom;
    i2:=I div Zoom;
    p1:=ShortInt(smp^[i2  ]);
    p2:=ShortInt(smp^[i2+1]);
    p3:=ShortInt(smp^[i2+2]);
    If i=0 then p0:=p1 Else p0:=ShortInt(smp^[i2-1]);
{16 bit calculations: }
          t:=p1 - p2;
          a:=(t shl 1 + t - p0 + p3);
          b:=(p2 shl 1 + p0) shl 1 - (p1 shl 2 + p1 + p3);
          c:=(p2 - p0);
{32 bit calculations: here are the original lines of code, for clarity...
          f2:=Integer(((LongInt(a) * fract) shl  3  + LongInt(b)  shl 19) shr 16);
          f2:=Integer((f2          * fract) shr 16) + LongInt(c)  shl 3;
          f2:=Integer((f2          * fract) shr 16) + LongInt(p1) shl 4;
{32 bit calculations: } { in 1 expression for better compiler optimisation: }
          f2:=Integer(((Integer((
              Integer(((LongInt(a) * fract) shl  3  + LongInt(b)  shl 19) shr 16)
              * fract) shr 16) + LongInt(c)  shl 3)
              * fract) shr 16) + LongInt(p1) shl 4;
          tmp:=f2 div $10;
{64 bit floating point calculations: - floating point alternative,
 the fastest way to do it in pascal:
    t:=p1 - p2;
    a:=(t shl 1 + t - p0 + p3) div 2;
    b:=p2 shl 1 + p0 - (p1 shl 2 + p1 + p3) div 2;
    c:=(p2 - p0) div 2;
    tmp:=Round((((a * f + b) * f) + c) * f + p1); }

    x2:=I;
    y2:=100-tmp;
    Line(x1, y1, x2, y2, Green);
    x1:=x2; y1:=y2;
  End;
END;



Var
  SineWave: array[0..320 div Zoom-1] of ShortInt;
  i: integer;
  ch: Char;

BEGIN
  Randomize;
  Set_VGA;
  Repeat
    Cls(0);
    For I:=0 to SizeOf(SineWave)-1 do SineWave[i]:=
                  Round(sin(I / 4)*30+random(30))+random(I div 2)-(I div zoom+1);
    DrawWave(@SineWave, SizeOf(SineWave));
    ch:=ReadKey;
    DrawLinear(@SineWave, SizeOf(SineWave));
    ch:=ReadKey;
    MONO_MIX_8BIT_IC(@SineWave, SizeOf(SineWave));
    ch:=ReadKey;
  Until ch=chr(27);
  TextMode(Co80);
END.
