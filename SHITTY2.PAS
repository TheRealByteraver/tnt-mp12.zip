{
  Copyright (c) 1998 Erland Van Olmen (erlandvo@hotmail.com)
  Protected Mode Ready
}
{$I-,N+,E-}
Unit Shitty2;


interface

{лллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллл}

Uses
  NewCrt;

Const
  MaxTopics = 16;

Type
  MenuTopicType = String[80];
  MenuType = Record
    Title: MenuTopicType;                           { Title of menu          }
    Topics: Array[0..MaxTopics-1] of MenuTopicType; { topic strings          }
    TAvail: Array[0..MaxTopics-1] of Boolean;       { topic avail? -=> True! }
    X1, Y1, X2, Y2: Byte;                           { physical Menu-coords   }
    NAvailC, TopicC, TopicBC: Byte;                 { colors                 }
    DefaultT: Byte;                                 { Default Topic          }
    BarPos  : Byte;                                 { Actual MenuBar Pos.    }
  End;

Const
  DefNAvailC = DarkGray;
  DefTopicC  = Black;
  DefTopicBC = LightGray;

  NoTopic = 255;

  Box7: Boxtype = (ord('л'), ord('л'), ord('п'), ord('п'), ord('п'), ord('л'), ord('п'), ord('л'));


Function  GetChoice(Menu: MenuType): Byte; { Returns NoTopic if user aborted }
Procedure DrawNiceBox(Menu: MenuType; BackGroundC: Byte);
Procedure DrawMenu(Menu: MenuType);
Procedure InitMenu(var Menu: MenuType);
Procedure ShowMenuBar(Menu: MenuType; Topic: Byte);
Procedure HideMenuBar(Menu: MenuType; Topic: Byte);
Procedure MoveMenuBar(Menu: MenuType; TopicStart, TopicEnd: Byte);
Function  TopicAbove (Menu: MenuType; Topic: Byte): Byte;  { These fn return }
Function  TopicBelow (Menu: MenuType; Topic: Byte): Byte;  { a topic if suc- }
                                                     { cessful or 255 if not }

{лллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллл}
{лллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллл}

implementation


Procedure WriteSP(x, y: Word; Msg: String; Len: Word);
VAR
  I: Byte;
BEGIN
  gotoxy(x, y); For I:=1 to Len do print(' '); gotoxy(x, y); print(Msg);
END;

{лллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллл}

Procedure DrawNiceBox(Menu: MenuType; BackGroundC: Byte);
VAR
  OldAttr, I: Byte;
BEGIN
  OldAttr:=TextAttr;
  With Menu do
  Begin
    TextBackGround(LightGray); TextColor(White);
    FilledBoxes:=True;  HBox(X1  , Y1  , X2  , Y2  , Box7);
    FilledBoxes:=False; LBox(X1+2, Y1+2, X2-2, Y2-1, Box1);
    TextBackGround(BackGroundC); TextColor(DarkGray);
    GotoXY(X1+1, Y2); For I:=1 to X2-X1 do print(Chr(Box7[7]));
    TextColor(White); GotoXY(X1 , Y2); print(Chr(Box7[3]));
  End;
  TextAttr:=OldAttr;
END;

{лллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллл}

Procedure DrawMenu(Menu: MenuType);
VAR
   OldAttr, I, Max: Byte;
BEGIN
  OldAttr:=TextAttr;
  With Menu do
  Begin
    TextColor(TopicC); TextBackGround(TopicBC);
    ClearXY(X1+2, Y1+2, X2-4, Y2-3);
    Gotoxy(X1+1, Y1+1);
    For I:=X1+1 to X2-1 do print(' ');
    Gotoxy(X1+(X2-X1-Length(Title)) shr 1, Y1+1);
    print(Title);
    If (Y2-Y1)>=MaxTopics then Max:=MaxTopics Else Max:=(Y2-Y1);
    For I:=0 to Max-2 do
    Begin
      If TAvail[I] then TextColor(TopicC) Else TextColor(NAvailC);
      Gotoxy(X1+4, Y1+3+I); print(Topics[I]);
    End;
    If TAvail[DefaultT] then ShowMenuBar(Menu, DefaultT)
    Else
    Begin
      I:=TopicBelow(Menu, 0);
      If I<>NoTopic then Begin BarPos:=I; ShowMenuBar(Menu, BarPos); End;
    End;
  End;
  TextAttr:=OldAttr;
END;

{лллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллл}

Procedure InitMenu(var Menu: MenuType);
VAR
   I: Byte;
BEGIN
  With Menu do Begin
    NAvailC:=DefNAvailC; TopicC:=DefTopicC; TopicBC:=DefTopicBC; DefaultT:=0;
    BarPos:=DefaultT;
    For I:=0 to MaxTopics-1 do Begin TAvail[I]:=False; Topics[I]:=''; End;
  End;
END;

{лллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллл}

Procedure ShowMenuBar(Menu: MenuType; Topic: Byte);
VAR
   OldAttr: Byte;
BEGIN
  If Topic>MaxTopics then exit;
  OldAttr:=TextAttr;
  TextColor(Menu.TopicBC); TextBackGround(Menu.TopicC);
  WriteSP(Menu.X1+3, Menu.Y1+3+Topic, ' '+Menu.Topics[Topic], Menu.X2-Menu.X1-5);
  TextAttr:=OldAttr;
END;

{лллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллл}

Procedure HideMenuBar(Menu: MenuType; Topic: Byte);
VAR
   OldAttr: Byte;
BEGIN
  If Topic>MaxTopics then exit;
  OldAttr:=TextAttr;
  TextColor(Menu.TopicC); TextBackGround(Menu.TopicBC);
  WriteSP(Menu.X1+3, Menu.Y1+3+Topic, ' '+Menu.Topics[Topic], Menu.X2-Menu.X1-5);
  TextAttr:=OldAttr;
END;

{лллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллл}

Procedure MoveMenuBar(Menu: MenuType; TopicStart, TopicEnd: Byte);
BEGIN
  If Menu.TAvail[TopicEnd] then
    HideMenuBar(Menu, TopicStart); ShowMenuBar(Menu, TopicEnd);
END;

{лллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллл}

Function TopicAbove (Menu: MenuType; Topic: Byte): Byte;
VAR
  I: Byte;
BEGIN
  If (Topic = 0) or (Topic>MaxTopics-1) then Begin TopicAbove:=NoTopic; exit; End;
  I:=Topic; Repeat Dec(I); Until (I=0) or (Menu.TAvail[I]);
  If Menu.TAvail[I] then TopicAbove:=I Else TopicAbove:=NoTopic;
END;

{лллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллл}

Function  TopicBelow (Menu: MenuType; Topic: Byte): Byte;
VAR
  I: Byte;
BEGIN
  If (Topic>=MaxTopics-1) then Begin TopicBelow:=NoTopic; exit; End;
  I:=Topic; Repeat Inc(I); Until (I>=MaxTopics-1) or (Menu.TAvail[I]);
  If Menu.TAvail[I] then TopicBelow:=I Else TopicBelow:=NoTopic;
END;

{лллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллл}

Function GetChoice(Menu: MenuType): Byte;  { Returns NoTopic if user aborted }
Const
  Enter     = chr(13);
  Escape    = chr(27);
  ArrowUp   = 'H';
  ArrowDown = 'P';

VAR
  Finished: Boolean;
  InsKey: Char;
  I: Byte;
BEGIN
  Finished:=False;
  InsKey  :='?';
  With Menu do
  Begin
    Repeat
      InsKey:=ReadKey;
      Case InsKey of
        Enter    : Begin Finished:=True; GetChoice:=BarPos;  Exit; End;
        Escape   : Begin Finished:=True; GetChoice:=NoTopic; Exit; End;
        ArrowUp  : Begin
                     I:=TopicAbove(Menu, BarPos);
                     If I<>NoTopic then
                       Begin MoveMenuBar(Menu, BarPos, I); BarPos:=I; End;
                   End;

        ArrowDown: Begin
                     I:=TopicBelow(Menu, BarPos);
                     If I<>NoTopic then
                       Begin MoveMenuBar(Menu, BarPos, I); BarPos:=I; End;
                   End;
      End;
    Until Finished;
    GetChoice:=BarPos;
  End;
END;

{лллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллл}
{лллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллл}

END.
