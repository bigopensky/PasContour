unit UTestContour;

{$mode objfpc}{$H+}

interface

uses
  Classes,
  SysUtils,
  Forms,
  Controls,
  Graphics,
  Dialogs,
  StdCtrls,
  ExtCtrls, ComCtrls,
  Math,
  BGRABitmap,
  BGRABitmapTypes,
  BGRACanvas2D,
  BGRACanvas,
  UDef,
  UProgress,
  UContour,
  ULogger,
  UGdalDriver;

Const cCutHeight  = 0.5;
      cDebug      = True;
      cOversample = 15;
      cMaxLoop    = 1;
      cSleepCnt   = 8;
      cScaleData  = 40;
      cScaleMaze  = 20;
      cOffsDraw   = 20;
      cSleepTime  = 1;

      cErrorStillSolving = 'Demo programm is solving a maze!';
      cErrorStillRunning = 'Demo programm is running a maze!';


type

  { TFrmMazeSolverDemo }

  TFrmMazeSolverDemo = class(TForm)
    btnRun: TButton;
    cbxDataSelect: TComboBox;
    chkInvertData: TCheckBox;
    chkWithCorners: TCheckBox;
    chkCorrectBorder: TCheckBox;
    lblProgress: TLabel;
    mmIo: TMemo;
    pgPainter: TPageControl;
    pnlBtn: TPanel;
    pnlData: TPanel;
    pnlMask: TPanel;
    splLog: TSplitter;
    tsMazeMatrix: TTabSheet;
    tsDataMatrix: TTabSheet;
    procedure btnChessClick(Sender: TObject);
    procedure btnMemoryClick(Sender: TObject);
    procedure btnRunClick(Sender: TObject);
    procedure cbxDataSelectChange(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure blTimeOut1Click(Sender: TObject);
    procedure pnlDataPaint(Sender: TObject);
    procedure pnlDataResize(Sender: TObject);
    procedure pnlMaskPaint(Sender: TObject);
    procedure pnlMaskResize(Sender: TObject);
  private
     fData: TDataMatrix;
     fSizeMx: TInteger;
     fSolver: TMazeSolver;
     fDataBitmap: TBGRABitmap;
     fMazeBitmap: TBGRABitmap;
     fStateRunning: TBoolean;
     fStateSolving: TBoolean;
     fIO: TLogger;
  public
    Procedure MazeProcessState(Sender: TObject);
    Procedure MazeCreated(Sender: TObject);
    Procedure MazeSolved(Sender: TObject);
    Procedure MazeAddWayPoint(Sender: TObject);
    Procedure MazeStartPath(Sender: TObject);
    Procedure MazeFinishPath(Sender: TObject);
    Procedure MazeCursorTurned(Sender: TObject);
    Procedure MazeCursorMoved(Sender: TObject);
    Procedure CheckMemory;
    Procedure DebugMemory(maxLoop: TInteger);
    Procedure SetDataChess;
    Procedure SetDataCross1;
    Procedure SetDataCross2;
    Procedure SetDataCrossHole;
    Procedure SetDataLargeHole;
    Procedure SetDataRandHole;
    Procedure SetDataRectBorder;
    Procedure SetDataSmallHole;
    Procedure SetDataSingles;
    Procedure SetDataComplex;
    procedure DrawMatrix(Sender: TObject);
    procedure DrawPoly(Sender: TObject);
    property  Log: TLogger  read fIO;
  end;

var
  FrmMazeSolverDemo: TFrmMazeSolverDemo;

implementation

{$R *.lfm}

function TrfmData(aReal: TReal): TInteger;
begin
 Result := round(cOffsDraw + aReal*cScaleData);
end;

function DataZellScaled: TInteger;
begin
   Result := cScaleData;
end;

function DataZellScaledCenter: TInteger;
begin
   Result := cScaleData div 2;
end;

function TrfmMaze(aReal: TReal): TInteger;
begin
 Result := round(cOffsDraw + aReal*cScaleMaze);
end;

function MazeZellScaled: TInteger;
begin
 Result := cScaleMaze;
end;

function MazeZellScaledCenter: TInteger;
begin
  Result := cScaleMaze div 2;
end;


//------------------------------------------------------------------------------
Procedure TFrmMazeSolverDemo.CheckMemory;
var
  b,a: THeapStatus;

begin
 fSolver.Clear;
 SetLength(fData, 0, 0);
 b:=GetHeapStatus;
 DebugMemory(10);
 a:=GetHeapStatus;
 writeln(
 '  BEFORE: ', a.TotalAllocated,
 ' AFTER: ', b.TotalAllocated,
 '  DIFF: ', a.TotalAllocated-b.TotalAllocated)
end;

procedure TFrmMazeSolverDemo.DebugMemory(maxLoop: TInteger);
begin

end;

//------------------------------------------------------------------------------
Procedure TFrmMazeSolverDemo.SetDataLargeHole;
 Begin
  fSizeMx:=5;
  SetLength(fData,fSizeMx);
  SetLength(fData,fSizeMx, fSizeMx);
  fData[0,0]:=0; fData[1,0]:=0; fData[2,0]:=1; fData[3,0]:=0; fData[4,0]:=0;
  fData[0,1]:=0; fData[1,1]:=1; fData[2,1]:=0; fData[3,1]:=1; fData[4,1]:=0;
  fData[0,2]:=1; fData[1,2]:=0; fData[2,2]:=0; fData[3,2]:=0; fData[4,2]:=1;
  fData[0,3]:=0; fData[1,3]:=1; fData[2,3]:=0; fData[3,3]:=1; fData[4,3]:=0;
  fData[0,4]:=0; fData[1,4]:=0; fData[2,4]:=1; fData[3,4]:=0; fData[4,4]:=0;
 End;

//------------------------------------------------------------------------------
Procedure TFrmMazeSolverDemo.SetDataRandHole;
 Begin
  fSizeMx:=5;
  SetLength(fData,fSizeMx, fSizeMx);
  fData[0,0]:=1; fData[1,0]:=0; fData[2,0]:=1; fData[3,0]:=0; fData[4,0]:=0;
  fData[0,1]:=0; fData[1,1]:=1; fData[2,1]:=0; fData[3,1]:=1; fData[4,1]:=0;
  fData[0,2]:=0; fData[1,2]:=0; fData[2,2]:=0; fData[3,2]:=0; fData[4,2]:=0;
  fData[0,3]:=0; fData[1,3]:=0; fData[2,3]:=0; fData[3,3]:=1; fData[4,3]:=0;
  fData[0,4]:=0; fData[1,4]:=0; fData[2,4]:=1; fData[3,4]:=0; fData[4,4]:=1;
 End;

//------------------------------------------------------------------------------
Procedure TFrmMazeSolverDemo.SetDataSingles;
 Begin
  fSizeMx:=5;
  SetLength(fData,fSizeMx, fSizeMx);
  fData[0,0]:=1; fData[1,0]:=0; fData[2,0]:=1; fData[3,0]:=0; fData[4,0]:=1;
  fData[0,1]:=0; fData[1,1]:=0; fData[2,1]:=0; fData[3,1]:=0; fData[4,1]:=0;
  fData[0,2]:=1; fData[1,2]:=0; fData[2,2]:=1; fData[3,2]:=0; fData[4,2]:=1;
  fData[0,3]:=0; fData[1,3]:=0; fData[2,3]:=0; fData[3,3]:=0; fData[4,3]:=0;
  fData[0,4]:=1; fData[1,4]:=0; fData[2,4]:=1; fData[3,4]:=0; fData[4,4]:=1;
 End;

//------------------------------------------------------------------------------
Procedure TFrmMazeSolverDemo.SetDataSmallHole;
 Begin
  fSizeMx:=5;
  SetLength(fData,fSizeMx, fSizeMx);
  fData[0,0]:=0; fData[1,0]:=0; fData[2,0]:=1; fData[3,0]:=0; fData[4,0]:=0;
  fData[0,1]:=0; fData[1,1]:=1; fData[2,1]:=2; fData[3,1]:=1; fData[4,1]:=0;
  fData[0,2]:=1; fData[1,2]:=2; fData[2,2]:=5; fData[3,2]:=2; fData[4,2]:=1;
  fData[0,3]:=0; fData[1,3]:=1; fData[2,3]:=2; fData[3,3]:=1; fData[4,3]:=0;
  fData[0,4]:=0; fData[1,4]:=0; fData[2,4]:=1; fData[3,4]:=0; fData[4,4]:=0;
 End;

//------------------------------------------------------------------------------
Procedure TFrmMazeSolverDemo.SetDataChess;
 Begin
  fSizeMx:=5;
  SetLength(fData,fSizeMx, fSizeMx);
  fData[0,0]:=1; fData[1,0]:=0; fData[2,0]:=1; fData[3,0]:=0; fData[4,0]:=1;
  fData[0,1]:=0; fData[1,1]:=1; fData[2,1]:=0; fData[3,1]:=1; fData[4,1]:=0;
  fData[0,2]:=1; fData[1,2]:=0; fData[2,2]:=1; fData[3,2]:=0; fData[4,2]:=1;
  fData[0,3]:=0; fData[1,3]:=1; fData[2,3]:=0; fData[3,3]:=1; fData[4,3]:=0;
  fData[0,4]:=1; fData[1,4]:=0; fData[2,4]:=1; fData[3,4]:=0; fData[4,4]:=1;
 End;

//------------------------------------------------------------------------------
Procedure TFrmMazeSolverDemo.SetDataCross1;
 Begin
  fSizeMx:=5;
  SetLength(fData,fSizeMx, fSizeMx);
  fData[0,0]:=1; fData[1,0]:=0; fData[2,0]:=1;fData[3,0]:=0;fData[4,0]:=1;
  fData[0,1]:=0; fData[1,1]:=1; fData[2,1]:=0;fData[3,1]:=1;fData[4,1]:=0;
  fData[0,2]:=0; fData[1,2]:=0; fData[2,2]:=1;fData[3,2]:=0;fData[4,2]:=0;
  fData[0,3]:=0; fData[1,3]:=1; fData[2,3]:=0;fData[3,3]:=1;fData[4,3]:=0;
  fData[0,4]:=1; fData[1,4]:=0; fData[2,4]:=1;fData[3,4]:=0;fData[4,4]:=1;
 End;

//------------------------------------------------------------------------------
Procedure TFrmMazeSolverDemo.SetDataCross2;
 Begin
  fSizeMx:=5;
  SetLength(fData,fSizeMx, fSizeMx);
  fData[0,0]:=0; fData[1,0]:=0; fData[2,0]:=1; fData[3,0]:=0; fData[4,0]:=0;
  fData[0,1]:=0; fData[1,1]:=0; fData[2,1]:=1; fData[3,1]:=0; fData[4,1]:=0;
  fData[0,2]:=1; fData[1,2]:=1; fData[2,2]:=1; fData[3,2]:=1; fData[4,2]:=1;
  fData[0,3]:=0; fData[1,3]:=0; fData[2,3]:=1; fData[3,3]:=0; fData[4,3]:=0;
  fData[0,4]:=0; fData[1,4]:=0; fData[2,4]:=1; fData[3,4]:=0; fData[4,4]:=0;
 End;

//------------------------------------------------------------------------------
Procedure TFrmMazeSolverDemo.SetDataCrossHole;
 Begin
  fSizeMx:=5;
  SetLength(fData,fSizeMx, fSizeMx);
  fData[0,0]:=0; fData[1,0]:=1; fData[2,0]:=1; fData[3,0]:=1; fData[4,0]:=0;
  fData[0,1]:=1; fData[1,1]:=1; fData[2,1]:=0; fData[3,1]:=1; fData[4,1]:=1;
  fData[0,2]:=1; fData[1,2]:=0; fData[2,2]:=0; fData[3,2]:=0; fData[4,2]:=1;
  fData[0,3]:=1; fData[1,3]:=1; fData[2,3]:=0; fData[3,3]:=1; fData[4,3]:=1;
  fData[0,4]:=0; fData[1,4]:=1; fData[2,4]:=1; fData[3,4]:=1; fData[4,4]:=0;
 End;

//------------------------------------------------------------------------------
procedure TFrmMazeSolverDemo.SetDataRectBorder;
 Begin
  fSizeMx:=5;
  SetLength(fData,fSizeMx, fSizeMx);
  fData[0,0]:=1; fData[1,0]:=1; fData[2,0]:=1; fData[3,0]:=1; fData[4,0]:=1;
  fData[0,1]:=1; fData[1,1]:=0; fData[2,1]:=0; fData[3,1]:=0; fData[4,1]:=1;
  fData[0,2]:=1; fData[1,2]:=0; fData[2,2]:=1; fData[3,2]:=0; fData[4,2]:=1;
  fData[0,3]:=1; fData[1,3]:=0; fData[2,3]:=0; fData[3,3]:=0; fData[4,3]:=1;
  fData[0,4]:=1; fData[1,4]:=1; fData[2,4]:=1; fData[3,4]:=1; fData[4,4]:=1;
 End;


//------------------------------------------------------------------------------
Procedure TFrmMazeSolverDemo.SetDataComplex;
 var
   lField: Array [1..20] of TString;
   lIx, lIy: TInteger;
   lStr: TString;
   lRc, LCc: TInteger;
   lMax : Integer = 0;
 Begin
  //           01234567890123456789
  lField[01]:=' 111111111111111111 ';
  lField[02]:=' 1                1 ';
  lField[03]:=' 1 11111111111111 1 ';
  lField[04]:=' 1 1            1 1 ';
  lField[05]:=' 1 1 1111111111 1 1 ';
  lField[06]:=' 1 1 1    11  1 1 1 ';
  lField[07]:=' 1 1 1111111111 1 1 ';
  lField[08]:=' 1 1            1 1 ';
  lField[09]:=' 1 11111111111111 1 ';
  lField[10]:=' 1                1 ';
  lField[11]:=' 111111111111111111 ';
  lField[12]:='                    ';
  lField[13]:=' 1111   1111111   1 ';
  lField[14]:='  1 1  1       1  1 ';
  lField[15]:=' 1111  1  111  1  1 ';
  lField[16]:='       1    1 1     ';
  lField[17]:='        1111111     ';
  lField[18]:='  11              1 ';
  lField[19]:='   111   11         ';
  lField[20]:='                  1 ';

  lMax := high(lField);
  for lIx:=low(lField) to high(lField) do
     if lMax < Length(lField[lIx]) then lMax := length(lField[lIx]);
  fSizeMx := lMax;
  SetLength(fData, fSizeMx, fSizeMx);
  lRc := 0;
  for lIx:= low(lField) to high(lField) do begin
    lStr:= lField[lIx];
    lCc:=0;
    for lIy:= 1 to Length(lStr) do begin
      if lStr[lIy] = ' ' then
          fData[lCc, lRc] := 0
      else
          fData[lCc, lRc] := 1;
      inc(lCc);
    end;
    inc(lRc);
  end;
 end;

//------------------------------------------------------------------------------
procedure TFrmMazeSolverDemo.FormCreate(Sender: TObject);
begin
  fDataBitmap := TBGRABitmap.Create;
  fMazeBitmap := TBGRABitmap.Create;
  fSolver:= TMazeSolver.Create;
  fSolver.OnMazeCreated  := @DrawMatrix;
  fIO := TLogger.Create(mmIo);

  fSolver.OnProgressTime   := @MazeProcessState;
  fSolver.OnPathStarted    := @MazeStartPath;
  fSolver.OnPathFinished   := @MazeFinishPath;
  fSolver.OnCursorMoved    := @MazeCursorMoved;
  fSolver.OnCursorTurned   := @MazeCursorTurned;
  fSolver.OnWayPointAdded  := @MazeAddWayPoint;
  fSolver.OnSolverFinished := @MazeSolved;
  SetDataChess;
  DrawMatrix(Self);
end;

//------------------------------------------------------------------------------
procedure TFrmMazeSolverDemo.FormShow(Sender: TObject);
begin

end;

procedure TFrmMazeSolverDemo.blTimeOut1Click(Sender: TObject);
begin

end;

//------------------------------------------------------------------------------
procedure TFrmMazeSolverDemo.pnlDataPaint(Sender: TObject);
begin
  fDataBitmap.Draw(pnlData.Canvas, 0, 0,True);
end;

procedure TFrmMazeSolverDemo.pnlDataResize(Sender: TObject);
begin
 fDataBitmap.SetSize(pnlData.Width, pnlData.Height);
 fDataBitmap.Canvas.Brush.Color:= clBlack;
 fDataBitmap.Canvas.FillRect(0, 0, fDataBitmap.Width, fDataBitmap.Height);
 DrawMatrix(self);
end;

//------------------------------------------------------------------------------
procedure TFrmMazeSolverDemo.pnlMaskPaint(Sender: TObject);
begin
 fMazeBitmap.Draw(pnlMask.Canvas, 0, 0,True);
end;

procedure TFrmMazeSolverDemo.pnlMaskResize(Sender: TObject);
begin
  fMazeBitmap.SetSize(pnlMask.Width, pnlMask.Height);
  fMazeBitmap.Canvas.Brush.Color:= clBlack;
  fMazeBitmap.Canvas.FillRect(0, 0, fMazeBitmap.Width, fMazeBitmap.Height);
  pnlMaskPaint(self);
end;

//------------------------------------------------------------------------------
procedure TFrmMazeSolverDemo.btnChessClick(Sender: TObject);
begin
  SetDataChess;
end;

//------------------------------------------------------------------------------
procedure TFrmMazeSolverDemo.btnMemoryClick(Sender: TObject);
begin
  CheckMemory;
end;

//------------------------------------------------------------------------------
procedure TFrmMazeSolverDemo.btnRunClick(Sender: TObject);
var
  lOptions : TMazeSolverOptions = [];
begin
  if fStateRunning then begin
      fIO.ErrorLn(cErrorStillRunning);
      exit;
  end;

  if fStateSolving then begin
      fIO.ErrorLn(cErrorStillSolving);
      exit;
  end;
  if chkWithCorners.Checked   then lOptions += [msoUseCorners];
  if chkInvertData.Checked    then lOptions += [msoInvertData];
  if chkCorrectBorder.Checked then lOptions += [msoCorrectBorder];
  pgPainter.ActivePage:= tsMazeMatrix;
  pnlMaskPaint(Self);
  cbxDataSelect.Enabled := FALSE;
  btnRun.Enabled    := FALSE;
  fStateRunning := True;
  fStateSolving := True;
  fSolver.Clear;
  fSolver.Solve(fData, cCutHeight, lOptions);
end;

//------------------------------------------------------------------------------
procedure TFrmMazeSolverDemo.cbxDataSelectChange(Sender: TObject);
begin
  if fStateSolving then begin
      fIO.ErrorLn(cErrorStillSolving);
      exit;
  end;
  case cbxDataSelect.Text of
       'SmallHole':  SetDataSmallHole;
       'Singles':    SetDataSingles;
       'Cross1':     SetDataCross1;
       'Cross2':     SetDataCross2;
       'CrossHole':  SetDataCrossHole;
       'Chess':      SetDataChess;
       'LargeHole':  SetDataLargeHole;
       'RectBorder': SetDataRectBorder;
       'Randhole':   SetDataRandHole;
       'Complex':    SetDataComplex;
  end;
  fDataBitmap.SetSize(pnlData.Width, pnlData.Height);
  fDataBitmap.Canvas.Brush.Color:= clBlack;
  fDataBitmap.Canvas.FillRect(0, 0, fDataBitmap.Width, fDataBitmap.Height);
  fMazeBitmap.Canvas.Brush.Color:= clBlack;
  fMazeBitmap.Canvas.FillRect(0, 0, fMazeBitmap.Width, fMazeBitmap.Height);
  pgPainter.ActivePage:=tsDataMatrix;
  pnlMaskPaint(self);
  pnlDataPaint(Self);
  DrawMatrix(Self);
end;

function MillieToStr( ms: TMillies): TString;
var
  ts: TTimeStamp;
  dt: TDateTime;
begin
     ts := MSecsToTimeStamp(ms);
     dt := TimeStampToDateTime(ts);
     Result := DateTimeToStr(dt);
end;

Procedure TFrmMazeSolverDemo.MazeProcessState(Sender: TObject);
var
  lNumStep, lNumReady, lNumLeft: TInteger;
  lPerc, lRate: TReal;
  lMsETA, lMsELA, lMsRun: TMillies;
begin
  with Sender as TProgress do begin
     GetState(lNumStep, lNumReady, lNumLeft,
           lPerc, lRate, lMsELA, lMsETA, lMsRun);
  end;
  lblProgress.Caption:= Format(
    'NUM: %d STEPS: %d LEFT: %d | '+
    'PART: %2.2f%% RATE: %2.f3 OPS/s |'+
    ' ETA: %2.1fs ELA: %2.1fs RUN: %2.1fs ',
    [ lNumReady, lNumStep, lNumLeft,
      lPerc, lRate,
      lMsEta/1000,
      lMsEla/1000,
      lMsRun/1000
     ] );
  Application.ProcessMessages;
end;

//------------------------------------------------------------------------------
procedure TFrmMazeSolverDemo.MazeCreated(Sender: TObject);
begin
  Log.OutLn('MAKE.MAZE...');
end;

//------------------------------------------------------------------------------
procedure TFrmMazeSolverDemo.MazeSolved(Sender: TObject);
begin
  Log.OutLn('.MAZE.SOLVED ');
  DrawMatrix(Self);
  DrawPoly(Self);
  fStateSolving := FALSE;
  fStateRunning := FALSE;
  cbxDataSelect.Enabled := TRUE;
  btnRun.Enabled    := TRUE;
  pgPainter.ActivePage:=tsDataMatrix;
  pnlDataPaint(Self);
end;

//------------------------------------------------------------------------------
procedure TFrmMazeSolverDemo.MazeAddWayPoint(Sender: TObject);
begin
 with Sender as TMazeSolver do begin
   if Assigned(CurWayPoint) then
      Log.OutLn('ADD.WAYPOINT: '+IntToStr(CurTrip.Count)
                                +':'+IntToStr(CurPath.Count))
   else
      Log.OutLn('ADD.WAYPOINT: ERROR');
 end;
end;

//------------------------------------------------------------------------------
procedure TFrmMazeSolverDemo.MazeStartPath(Sender: TObject);
begin
 with Sender as TMazeSolver do
    Log.OutLn('START.PATH:  '+IntToStr(CurTrip.Count));
end;

//------------------------------------------------------------------------------
procedure TFrmMazeSolverDemo.MazeFinishPath(Sender: TObject);
var
   lIndex: TInteger = 0;
   lHelp:  TString;
begin
  with Sender as TMazeSolver do begin
    if CurPath <> NIL then lIndex:=sign(CurPath.WindingIndex);
    lHelp := '';
    case lIndex of
         0: lHelp := 'INIT.PATH:  ';
         1: lHelp := 'OUTER.PATH: ';
        -1: lHelp := 'INNER.PATH: ';
    end;
    Log.OutLn('FIN.'+lHelp+IntToStr(CurTrip.Count));
  end;
end;

//------------------------------------------------------------------------------
procedure TFrmMazeSolverDemo.MazeCursorTurned(Sender: TObject);
begin
 with Sender as TMazeSolver do
   Log.OutLn('TURN.CURSOR: '+CursorToString(CurCursor));
end;

//------------------------------------------------------------------------------
procedure TFrmMazeSolverDemo.MazeCursorMoved(Sender: TObject);
begin
  with Sender as TMazeSolver do
    Log.OutLn('MOVE.CURSOR: '+CursorToString(CurCursor));
  DrawMatrix(Sender);
end;

//------------------------------------------------------------------------------
// TODO remove deep nesting
// -----------------------------------------------------------------------------
procedure TFrmMazeSolverDemo.DrawMatrix(Sender: TObject);
var
  lDataMx: TDataMatrix;
  lMazeMx: TMazeMatrix;
  lCol, lRow: TInteger;
  lX1, lY1, lX2, lY2: TInteger;
  lColor: TColor;
  lHgt: TReal;
  lCnt: TInteger = 0;
  lTxt: TInteger = 0;
  lPoly:TMazePath;
  lWayPoint: TWayPoint;
begin
   { If the sender is the form, draw  the data matrix }
   if Sender = Self then begin
      fDataBitmap.SetSize(pnlData.Width, pnlData.Height);
      with fDataBitmap.Canvas do begin
        Brush.Color:= clBlack;
        FillRect(0, 0, Width, Height);
        lDataMx:= fData;
        for lCol:= low(lDataMx) to high(lDataMx) do begin
           for lRow := low(lDataMx[lCol]) to high(lDataMx[lCol]) do begin
             lX1:= TrfmData(lCol); lX2:= TrfmData(lCol)+DataZellScaled - 1;
             lY1:= TrfmData(lRow); lY2:= TrfmData(lRow)+DataZellScaled - 1;
             lHgt := lDataMx[lCol, lRow] ;
             if lHgt < cCutHeight then lColor := clWhite else lColor:= clGray;
             Pen.Color:= clMaroon;
             Brush.Color:= lColor;
             FillRect (lX1, lY1, lX2, lY2);
             Rectangle(lX1, lY1, lX2, lY2);
           end; // for lCol
        end; // for lRow
      end; // with mBmpDataCanvas do
      pnlDataPaint(self);
   end;

   { ---- Begin draw the maze matrix, cursor and waypoints --- }
   if Sender = fSolver then begin
     lMazeMx:= fSolver.Maze;
     fMazeBitmap.SetSize(pnlMask.Width, pnlMask.Height);
     with fMazeBitmap.Canvas do begin
         Font.Name:='Monospace';
         Font.Size:=8;
         Brush.Color:= clBlack;
         FillRect(0, 0, Width, Height);

         { --- Begin draw the cells --- }
         for lCol:= low(lMazeMx) to high(lMazeMx) do begin
            for lRow := low(lMazeMx[lCol]) to high(lMazeMx[lCol]) do begin
              lX1:= TrfmMaze(lCol); lX2:= TrfmMaze(lCol)+MazeZellScaled-1;
              lY1:= TrfmMaze(lRow); lY2:= TrfmMaze(lRow)+MazeZellScaled-1;
              case lMazeMx[lCol, lRow] of
                   mcsWall:      lColor:=clGray;
                   mcsCorridor:  lColor:=clWhite;
                   mcsVisited:   lColor:=clGreen;
                  // mstNeigbor: lColor:=clYellow;
              end; // case
              if (lColor<>clGreen) then begin

                 if (lCol < 2 ) or
                   (lCol > high(lMazeMx)-2) then lColor := clLtGray;

                if (lRow < 2 ) or
                   (lRow > high(lMazeMx[lCol])-2) then lColor := clLtGray;

              end; // if lColor
              Pen.Color:= clMaroon;
              Brush.Color:= lColor;
              FillRect(lX1, lY1, lX2, lY2);
              Rectangle(lX1, lY1, lX2, lY2);
          end; // for lRow
       end; // for lCol
       { --- End draw the cells --- }

       { --- Begin draw cursor --- }
        lX1:=TrfmMaze(fSolver.CurCursor.col) + MazeZellScaledCenter;
        lY1:=TrfmMaze(fSolver.CurCursor.row) + MazeZellScaledCenter;
        case fSolver.CurCursor.dir of
             MV_WEST: begin
               lX2 := fSolver.CurCursor.col - 1;
               lY2 := fSolver.CurCursor.row;
             end;
             MV_SOUTH: begin
               lX2 := fSolver.CurCursor.col;
               lY2 := fSolver.CurCursor.row + 1;
             end;
             MV_EAST: begin
               lX2 := fSolver.CurCursor.col + 1;
               lY2 := fSolver.CurCursor.row;
             end;
             MV_NORTH: begin
               lX2 := fSolver.CurCursor.col;
               lY2 := fSolver.CurCursor.row - 1;
             end;
        end;
        lX2 := TrfmMaze(lX2) + MazeZellScaledCenter;
        lY2 := TrfmMaze(lY2) + MazeZellScaledCenter;
        Pen.Color:= clNavy;
        Pen.Width:= 2;
        MoveTo(lX1, lY1);
        LineTo(lX2, lY2);
        case fSolver.CurCursor.dir of
             MV_WEST: begin
               lX2 := fSolver.CurCursor.col;
               lY2 := fSolver.CurCursor.row + 1;
             end;
             MV_SOUTH: begin
               lX2 := fSolver.CurCursor.col + 1;
               lY2 := fSolver.CurCursor.row;
             end;
             MV_EAST: begin
               lX2 := fSolver.CurCursor.col;
               lY2 := fSolver.CurCursor.row - 1;
             end;
             MV_NORTH: begin
               lX2 := fSolver.CurCursor.col - 1;
               lY2 := fSolver.CurCursor.row;
             end;
        end;
        lX2 := TrfmMaze(lX2) + cScaleMaze div 2;
        lY2 := TrfmMaze(lY2) + cScaleMaze div 2;
        Ellipse(lX1-MazeZellScaledCenter,
                lY1-MazeZellScaledCenter,
                lX1+MazeZellScaledCenter,
                lY1+MazeZellScaledCenter);
        TextOut(lX1-cScaleMaze div 6,
                lY1-cScaleMaze div 6,
                IntToStr(fSolver.CurCursor.dir));

        Pen.Color:= clYellow;
        Pen.Width:= 4;
        MoveTo(lX1, lY1);
        LineTo(lX2, lY2);
        { --- End draw cursor --- }

        { --- Begin draw vertices --- }
        Pen.Width:= 2;
        Pen.Color:= clRed;
        if fSolver.CurTrip <> NIL then begin
          for lPoly in fSolver.CurTrip do begin
            for lTxt:=0 to lPoly.Count-1 do begin
              lWayPoint:=lPoly[lTxt];
              lX1 := TrfmMaze(lWayPoint.WayCol*2+2)  + MazeZellScaledCenter;
              lY1 := TrfmMaze(lWayPoint.WayRow*2+2)  + MazeZellScaledCenter;
              lX2 := TrfmMaze(lWayPoint.WallCol*2+2) + MazeZellScaledCenter;
              lY2 := TrfmMaze(lWayPoint.WallRow*2+2) + MazeZellScaledCenter;
              MoveTo(lX1, lY1);
              LineTo(lX2, lY2);
              Ellipse(lX1-cScaleMaze div 4, lY1-cScaleMaze div 4,
                      lX1+cScaleMaze div 4, lY1+cScaleMaze div 4);
              TextOut(lX1-cScaleMaze div 3, lY1-cScaleMaze div 3,
                      IntToStr(lTxt));
            end; // for lTxt
          end;  // for lPoly
        end;  // if fSolver.lCurTrip <> NIL
        { --- End draw vertices --- }
     end; // with bmpMazeCanvas do

      { paint the result into the graphix panel }
      pnlMaskPaint(self);
      fStateRunning:=False;

      { Pause process for animation }
      while not fStateRunning and not (lCnt > cSleepCnt) do begin
         Application.ProcessMessages;
         sleep(cSleepTime);
         inc(lCnt);
      end;
      { Flag to go on }
      fStateRunning := True;
   end; // if Sender = fSolver
   { ---- End draw the maze matrix, cursor and waypoints --- }

end;

//------------------------------------------------------------------------------
procedure TFrmMazeSolverDemo.DrawPoly(Sender: TObject);
var
  lFace:    TFace;
  lPoly:    TRing;
  lVertex:  TVertex;
  lX, lY:   TInteger;
  lPolyFillCol: TBGRAPixel;
  lPolyLineCol: TBGRAPixel;
  lRectLineCol: TBGRAPixel;
begin
  { Create the faces from the current contour set }
  lFace := fSolver.ExtractFace;
  try

    { Set the colors }
    with lPolyFillCol do begin
      red   := 130;
      green := 230;
      blue  := 255;
      alpha := 128;
    end;

    with lPolyLineCol do begin
      red   := 0;
      green := 0;
      blue  := 128;
      alpha := 128;
    end;

    with lRectLineCol do begin
      red   := 255;
      green := 0;
      blue  := 0;
      alpha := 128;
    end;

    { Stroke and fill the polygon path,
      We need path ops here, so we use the 2D canvas }
    with fDataBitmap.Canvas2D do begin
       fillStyle(lPolyFillCol);
       strokeStyle(lPolyLineCol);
       beginPath;
       for lPoly in lFace do begin
          for lVertex in lPoly do begin
             lX := TrfmData(lVertex.X);
             lY := TrfmData(lVertex.Y);
             if lVertex = lPoly.First then
                moveTo(lX, lY)
             else
                LineTo(lX, lY);
          end;
          lVertex := lPoly.First;
          lX := TrfmData(lVertex.X);
          lY := TrfmData(lVertex.Y);
          LineTo(lX, lY);
       end;
       closePath;
       fill;
       stroke();
    end; // With BGRACanvas2D do

    { Draw small rectangle marker around the verices.
      Using plain rectangle her, using plain BGRACanvas only
    }
    with fDataBitmap.CanvasBGRA do begin
      Pen.Color:=lRectLineCol;
      Pen.Width:=3;
      for lPoly in lFace do begin
        for lVertex in lPoly do begin
           Rectangle(
             TrfmData(lVertex.X)-1, TrfmData(lVertex.Y)-1,
             TrfmData(lVertex.X)+1, TrfmData(lVertex.Y)+1, FALSE
            );
        end; // for lPoly
      end; //for lVerex
    end; // with CanvasBGRA do

  finally
    { Free the created faces }
    lFace.Free;
  end;
  { Update the graphix panel panel }
  pnlDataPaint(Self);
end;

end.

