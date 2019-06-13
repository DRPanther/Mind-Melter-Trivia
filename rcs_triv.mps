Uses Cfg;
Uses User;

Const
  bbs  = 'Castle Rock BBS'            //BBS Name :)
  MBaseID = '1010'                      //Message base ID to post message
  rcspath = '/home/dan/mystic/scripts/rcs/trivdoor/' //Path to all data files
  prog = 'RCS 80s Music Trivia'
  ver  = 'v0.01'

Type
  Rec = Record
    QNum   : String;
    Quest  : Array[1..10] of String[80];
    QOpt   : Array[1..4] of String[60];
    QAns   : Array[1..30] of String[80];
    QA     : Char
  End;

Type
  scores = Record
    index   : byte
    bbses   : String
    name    : String
    alias   : String
    score   : byte
    lastplay: LongInt
End

Var
  AppDir    : String;
  DatFile   : String;
  PlyrCount : Integer=0
  ibbsplyr  : Integer=0
  bool      : Boolean;
  ch        : Char
  score     : scores
  ibbsscore : scores
  skor      : Array[1..50] of scores
  ibbsskor  : Array[1..75] of scores
  skorfile  : String
  skoribbs  : String
  TrivSkor  : String
  ts        : file

procedure proginit
Begin
  GetThisUser;
  AppDir :=  AddSlash(rcspath);
  DatFile := AddSlash(AppDir)+'rcstriv.dat';
  SkorFile:= AddSlash(AppDir)+'trivskor.dat';
  Skoribbs:= AddSlash(AppDir)+'trivibbs.dat';
  TrivSkor:= AddSlash(AppDir)+'trivscor.txt';
End

Function ReadPlyr(I:Integer):Boolean                //read player info
Var Ret   : Boolean = False
Var Fptr  : File
Begin
  fAssign(Fptr,skorfile,66)
  fReset(Fptr)
  If IOResult = 0 Then Begin
    fSeek(Fptr,(I-1)*SizeOf(score))
    If Not fEof(Fptr) Then Begin
      fReadRec(Fptr,score)
      Ret:=True
    End
    fClose(Fptr)
  End
  ReadPlyr:=Ret
End

Function ReadIBBS(I:Integer):Boolean                //read player info
Var Ret   : Boolean = False
Var Fptr  : File
Begin
  fAssign(Fptr,skoribbs,66)
  fReset(Fptr)
  If IOResult = 0 Then Begin
    fSeek(Fptr,(I-1)*SizeOf(ibbsscore))
    If Not fEof(Fptr) Then Begin
      fReadRec(Fptr,ibbsscore)
      Ret:=True
    End
    fClose(Fptr)
  End
  Readibbs:=Ret
End

Procedure SaveIbbs(I:Integer)                  //save player's record
Var Ret  : Boolean = False
Var Fptr  : File
Begin
  fAssign(Fptr,skoribbs,66)
  fReset(Fptr)
  If IOResult = 0 Then
    fSeek(Fptr,(I-1)*SizeOf(ibbsscore))
  Else Begin
    ibbsscore.Index:=1
    fRewrite(Fptr)
  End
  fWriteRec(Fptr,ibbsscore)
  fClose(Fptr)
End

Procedure SavePlyr(I:Integer)                  //save player's record
Var Ret  : Boolean = False
Var Fptr  : File
Begin
  fAssign(Fptr,skorfile,66)
  fReset(Fptr)
  If IOResult = 0 Then
    fSeek(Fptr,(I-1)*SizeOf(score))
  Else Begin
    score.Index:=1
    fRewrite(Fptr)
  End
  fWriteRec(Fptr,score)
  fClose(Fptr)
End

Function FindIBBS(RN,IBBS:String):Integer          //find player in .dat file
Var Ret  : Integer = 0
Var I    : Integer = 1
Begin
  RN:=Upper(RN)
  IBBS:=Upper(IBBS)
  While ReadIBBS(I) And Ret = 0 Do Begin
    If (Upper(ibbsscore.Name)=RN)and(Upper(ibbsscore.bbses)=IBBS) Then
      Ret:=ibbsscore.Index
    I:=I+1
  End
  FindIBBS:=Ret
End

Function FindPlyr(RN:String):Integer          //find player in .dat file
Var Ret  : Integer = 0
Var I    : Integer = 1
Begin
  RN:=Upper(RN)
  While ReadPlyr(I) And Ret = 0 Do Begin
    If Upper(score.Name)=RN Then
      Ret:=score.Index
    I:=I+1
  End
  FindPlyr:=Ret
End

Procedure listlocalscores
Var
  x         : byte=1
  y         : byte=1
  temp      : byte
  lastrecord:byte=1
Begin
  While ReadPlyr(x) Do
  Begin
    skor[x].index:=score.index
    skor[x].alias:=score.alias
    skor[x].name:=score.name
    skor[x].bbses:=score.bbses
    skor[x].score:=score.score
    skor[x].lastplay:=score.lastplay
    x:=x+1
    lastrecord:=lastrecord+1
  End
  temp:=lastrecord+1
  For x:=1 to lastrecord do
  Begin
    For y:=1 to lastrecord do
    Begin
      if skor[x].score > skor[y].score then
      Begin
        skor[temp]:=skor[x]
        skor[x]:=skor[y]
        skor[y]:=skor[temp]
      End
    End
  End
  ClrScr
  WriteLn('')
  WriteLn(PadCt('Local BBS Scores',78,' '))
  WriteLn('')
  WriteLn(' Alias                           Score')
  WriteLn('')
  For x:=1 to lastrecord-1 do
  Begin
    If skor[x].score<>0 then
    Begin
      WriteLn(skor[x].alias+'          '+Int2Str(skor[x].score))
    End
  End
  pause
End

procedure listibbsscores
Var
  x         : byte=1
  y         : byte=1
  temp      : byte
  lastrecord:byte=1
Begin
  While ReadIBBS(x) Do
  Begin
    ibbsskor[x].index:=ibbsscore.index
    ibbsskor[x].alias:=ibbsscore.alias
    ibbsskor[x].name:=ibbsscore.name
    ibbsskor[x].bbses:=ibbsscore.bbses
    ibbsskor[x].score:=ibbsscore.score
    ibbsskor[x].lastplay:=ibbsscore.lastplay
    x:=x+1
    lastrecord:=lastrecord+1
  End
  temp:=lastrecord+1
  For x:=1 to lastrecord do
  Begin
    For y:=1 to lastrecord do
    Begin
      if ibbsskor[x].score > ibbsskor[y].score then
      Begin
        ibbsskor[temp]:=ibbsskor[x]
        ibbsskor[x]:=ibbsskor[y]
        ibbsskor[y]:=ibbsskor[temp]
      End
    End
  End
  ClrScr
  WriteLn('')
  WriteLn(PadCt('InterBBS Scores',78,' '))
  WriteLn('')
  WriteLn(' Alias                 BBS                Score')
  WriteLn('')
  For x:=1 to lastrecord-1 do
  Begin
    If ibbsskor[x].score<>0 then
    Begin
      WriteLn(ibbsskor[x].alias+'         '+ibbsskor[x].bbses+'         '+Int2Str(ibbsskor[x].score))
    End
  End
  pause
End

function Rot47(s: string): string;
var
  i, j: integer;
  res : String;
begin
  Res := s;
  for i := 1 to Length(s) do
  begin
    j := Ord(s[i]);
    if (j>=33) and (j<=126) then
    begin
      Res[i] := Chr(33 + ((j + 14) % 94));
    end;
  end;
  Rot47:=Res;
end;

Procedure GetDataFrom(S:String);
Var
  fp : File;
  l  : String;
  U  : Rec;
  R  : Rec;
  x  : Integer;
  y  : Byte=1
  temp: scores
Begin
  x:=1;
  If Not FileExist(S) Then Exit;
  FillChar(U,SizeOf(U),#0);
  fAssign(fp,S,66);
  fReset(fp);
  While Not fEOF(fp) Do Begin
    fReadLn(fp,l);
    if l='>>>BEGIN' Then Begin
      fReadLn(fp,l);
      U.QNum:=Rot47(l);
      fReadLn(fp,l);
      Repeat
        U.Quest[x]:=Rot47(l);
        x:=x+1;
        fReadLn(fp,l);
      Until ((l[1]='2')and(l[2]=']'))
      x:=1;
      For x:=1 to 4 do
      begin
        U.QOpt[x]:=Rot47(l);
        fReadLn(fp,l);
      end;
      x:=1
      fReadLn(fp,l)
      Repeat
        if WordGet(1,l,' ')='Y' then
        Begin
          U.QAns[x]:=Rot47(l)
          x:=x+1
        End
        fReadLn(fp,l)
      Until (l='')or(x=14)
    End
    if l='>>> END' Then continue;
    if l='>>>START' Then
    Begin
      Repeat
      fReadLn(fp,l)
      if y=1 then temp.name:=Rot47(l)
      if y=2 then temp.alias:=Rot47(l)
      if y=3 then temp.bbses:=Rot47(l)
      if y=4 then temp.score:=Str2Int(Rot47(l))
      if y=5 then temp.lastplay:=Str2Int(Rot47(l))
      y:=y+1
      Until l='>>>FINISH'
    End
  End;
  SaveIbbs(FindIBBS(temp.name,temp.bbses))
  U.QA:=WordGet(2,U.QAns[1],' ')
  fClose(fp);
  FileErase(S);

  fAssign(fp,DatFile,66);
  If FileExist(DatFile) Then Begin
    fReset(fp);
    if U.QNum <> '' then fWriteRec(fp,U)
  End Else Begin
    fReWrite(fp);
    fWriteRec(fp,U)
  End;
  fClose(fp)
  fReset(fp)
  While Not FEOF(fp) Do Begin
    FRead(fp,R,SizeOf(R));
    If U.QNum = R.QNum then Begin
      If U.Quest[1] = R.Quest[1] then bool:=true;
    End;
  End;
  If bool=false then fWriteRec(fp,U);
  fClose(fp);
End;

Function Checkans(x,y:char):Boolean
Var
  ret:boolean=false
Begin
  if Upper(x)=Upper(y) then ret:=true
  Checkans:=ret
End

Procedure ShowData;
Var
  fp  : File;
  U   : Rec;
  i   : integer=1;
Begin
  score.bbses:=bbs
  SavePlyr(score.index)
  If Not FileExist(DatFile) Then Begin
    ClrScr;
    WriteLn('No Data to display... Exiting.');
    Pause;
    Exit;
  End;
  DispFile(AppDir+'trivques.ans')
  fAssign(fp,DatFile,66);
  fReset(fp);
  If IOResult = 0 then
  Begin
    fReadRec(fp,U);
  End
  Else halt
  WriteXY(4,3,15,U.QNum);
  Repeat
    WriteXY(3,7+i,15,U.Quest[i])
    i:=i+1
  Until (U.Quest[i]='')and(U.Quest[i+1]='')
  For i:=1 to 4 do
  Begin
    WriteXY(3,17+i,15,U.QOpt[i])
  End
  fClose(fp);
  WriteXY(3,23,15,'Enter your choice '+score.alias+' (Q-Quit) : ')
  ch:=Upper(OneKey('ABCDQ',True))
  if ch='Q' then break
  if not Checkans(ch,U.QA) then
  Begin
    DispFile(AppDir+'trivwron.ans')
    ReadKey
  End
  Else
  Begin
    score.score:=score.score+1
    score.lastplay:=DateTime
    SavePlyr(score.index)
    DispFile(AppDir+'trivcorr.ans')
    ReadKey
  End
  DispFile(AppDir+'trivansw.ans')
  WriteXY(4,3,15,U.QNum)
  i:=1
  Repeat
    WriteXY(3,7+i,15,U.QAns[i])
    i:=i+1
  Until WordGet(1,U.QAns[i],' ')=''
  GotoXY(1,24)
  ReadKey
End;

procedure readfiles
Begin
  FindFirst (AddSlash(AppDir)+'*.trv', 66);
  While DosError = 0 Do
    Begin
      GetDataFrom(AddSlash(AppDir)+DirName);
      FindNext;
    End;
  FindClose;
End

Function datecheck(t,p:longint):Boolean
Var
  date1day : word
  date1mon : word
  date1yer : word
  date2day : word
  date2mon : word
  date2yer : word
  ret      : boolean=false
Begin
  date1day:=Str2Int(WordGet(2,DateStr(t,1),'/'))
  date1mon:=Str2Int(WordGet(1,DateStr(t,1),'/'))
  date1yer:=Str2Int(WordGet(3,DateStr(t,1),'/'))
  date2day:=Str2Int(WordGet(2,DateStr(p,1),'/'))
  date2mon:=Str2Int(WordGet(1,DateStr(p,1),'/'))
  date2yer:=Str2Int(WordGet(3,DateStr(p,1),'/'))
  if date1yer>date2yer then ret:=true
  else if date1mon>date2mon then ret:=true
  else if date1mon=date2mon then
  begin
    if date1day>date2day then ret :=true
  End
  datecheck:=ret
End

procedure NewPlyr
Begin
  PlyrCount:=PlyrCount+1
  score.index:=PlyrCount
  score.name:=UserName
  score.Alias:=UserAlias
  score.bbses:=bbs
  score.score:=0
  score.lastplay:=DateTime
  SavePlyr(score.index)
End

procedure progend
Begin
  SavePlyr(score.index)
  //fWriteLn(ts,Int2Str(score.index))
  fAssign(ts,trivskor,66)
  fReWrite(ts)
  fWriteLn(ts,'>>>START')
  fWriteLn(ts,Rot47(score.name))
  fWriteLn(ts,Rot47(score.alias))
  fWriteLn(ts,Rot47(score.bbses))
  fWriteLn(ts,Rot47(Int2Str(score.score)))
  fWriteLn(ts,Rot47(Int2Str(score.lastplay)))
  fWriteLn(ts,'>>>FINISH')
  fClose(ts)
  If FileExist(AppDir+'trivscor.txt') then
  Begin
    MenuCmd('MX',AppDir+'trivscor.txt'+';'+MBaseID+';TriviaMaster;All;80s_Music_Trivia');
    //FileErase(AppDir+'trivscor.txt');
  End
  GotoXY(1,22)
End

procedure main
Var
  x    : Char=''
  y    : Integer
  done : Boolean=false
Begin
  ClrScr
  DispFile(AppDir+'trivspls.ans')
  WriteXY(27,20,15,prog+' '+ver)
  ReadKey
  Repeat
    DispFile(AppDir+'trivansw.ans')
    WriteXY(24,10,15,'(P)lay Trivia')
    WriteXY(24,12,15,'(L)ist Players Scores')
    WriteXY(24,14,15,'(Q)uit back to BBS')
    GotoXY(1,24)
    x:=Upper(OneKey('PLQ',False))
    Case x Of
      'P': Begin
             y:=FindPlyr(UserName)
             if y > 0 Then
             Begin
               if datecheck(DateTime,score.lastplay) then
               Begin
                 ReadPlyr(y)
                 readfiles
                 ShowData
               End
               Else
               Begin
                 WriteLn('You have already played today...')
                 pause
               End
             End
             Else
             Begin
               NewPlyr
               readfiles
               ShowData
             End
           End
      'L': Begin
             listlocalscores
             listibbsscores
           End
      'Q': done:=true
    End
  Until done
End

Begin
  proginit
  readfiles
  main
  progend
End;
