Uses
  NewCrt;
Var
  I: integer;
  ch: char;
Begin
{  For I:=0 to 80*50-1 do Mem[SegB800:I*2+1]:=I mod 256;}
  GotoXY(50, 1);
  clrscr;
  println('Hit ESC to exit, any other key for testing');
  putc(50, 20, Ord('õ'));
{  If Byte(Ptr(0, $449)^) = Mono then writeLn('Mono') Else WriteLn('Color');}
  Repeat
    If Keypressed then
    Begin
      ch:=readkey;
      println(ch+' key pressed.');
    end;
  Until ch = chr(27);
End.