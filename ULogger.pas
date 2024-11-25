unit ULogger;

{$mode objfpc}{$H+}

interface

uses
  Classes,
{$ifdef lcl}
  Dialogs,
  StdCtrls,
{$endif}
  SysUtils,
  UDef;


const
  MM_OUT_MIN_LINE = 10;
  MM_OUT_MAX_LINE = 10000;

  MM_OUT_DEBUG_PFX = 'DEBUG: ';
  MM_OUT_ERROR_PFX = 'ERROR: ';
  MM_OUT_OUT_PFX   = '';
  MM_OUT_INFO_PFX  = 'INFO:  ';
  MM_OUT_WARN_PFX  = 'WARN:  ';
  MM_OUT_ECHO_PFX  = 'ECHO:  ';

type
  TIOChannel = (IO_ECHO, IO_OUT, IO_INFO, IO_WARN, IO_DEBUG, IO_ERROR);

  {$ifdef lcl}
  TGuiConsole = Class(TObject)
    protected
      fMM:TMemo;
    public
      constructor Create(aMemo:TMemo);
      destructor  Destroy; override;
      procedure   Clear;
      procedure   CheckLimit(aLimit: TInteger);
      procedure   AppendStdErr(msg:  TString);
      procedure   AppendStdOut(msg:  TString);
  end;
  {$else}
  TTextConsole = Class(TObject)
    public
      fStdErrLineCount:Integer;
      fStdOutLineCount:Integer;
      constructor create;
      destructor destroy; override;
      procedure Clear;
      procedure CheckLimit(aLimit: TInteger);
      procedure AppendStdErr(msg:  TString);
      procedure AppendStdOut(msg:  TString);
  end;
  TIOClass = TTextConsole;
  {$endif}

  TLogger = class(TObject)
  private
    {$ifdef lcl}
    fMmPort:    TGuiConsole;
    {$else}
    fMmPort:    TTextConsole;
    {$endif}
    fFile:String;
    fUseErrorDialog: boolean;
    fUseException: boolean;
    fLineLimit: integer;
    fDebug:     integer;
    fLog:       Boolean;
    fText:      TextFile;
    fPrompt:    String;
    procedure SelectOut (aMsg: string; aChn: TIOChannel);
    procedure CheckLineLimit;
  public
    {$ifdef lcl}
    constructor Create (aIoClass: TMemo; useExcept: TBoolean = FALSE);
    {$else}
    constructor Create;
    {$endif}
    destructor Destroy; override;
    function   GetDebugLevel: TInteger;
    procedure  SetDebugLevel (aLvl: TInteger);
    function   GetLineLimit: TInteger;
    procedure  SetLineLimit (aLmt: TInteger);
    function   GetErrorDialogUsage: TBoolean;
    procedure  OpenLog(aName: TString);
    procedure  CloseLog;
    procedure  SetUseErrorDialog (aUseDlg: TBoolean);
    procedure  DebugLn (aLvl: TInteger; aMsg:  TString);
    procedure  DebugLn (aLvl: TInteger; aMsgs: TStrings);
    procedure  DebugFmt (aLvl: TInteger; aFmt:  TString; aArgs: array of const);
    procedure  OutLn (aMsg:  TString);
    procedure  OutLn (aMsgs: TStrings);
    procedure  OutFmt (aFmt: TString; aArgs: array of const);
    procedure  InfoLn (aMsg:  TString);
    procedure  InfoLn (aMsgs: TStrings);
    procedure  InfoFmt (aFmt: TString; aArgs: array of const);
    procedure  Echo (aMsg:   TString; useDlg: TBoolean = FALSE);
    procedure  EchoFmt (aFmt: TString; aArgs: array of const; useDlg: TBoolean = FALSE);
    procedure  ErrorLn (aMsg: TString);
    procedure  ErrorLn (aMsgs: array of TString);
    procedure  ErrorFmt (aFmt: TString; aArgs: array of const);
    procedure  InfoPopup(aMsg: TString);
    procedure Clear;
    {$ifdef lcl}
    property console: TGuiConsole read fMmPort;
    {$else}
    property console: TTextConsole read fMmPort;
    {$endif}
    property  prompt: String read fPrompt write fPrompt;
  end;


implementation

{$ifdef lcl}
constructor TGuiConsole.create(aMemo:TMemo);
begin
  inherited create;
  fMM:= aMemo;
end;

destructor TGuiConsole.destroy;
begin
  inherited destroy;
end;

procedure TGuiConsole.AppendStdErr(msg: TString);
begin
  fMM.lines.BeginUpdate;
  fMM.lines.append(msg);
  fMM.lines.EndUpdate;
  fMM.SelStart := Length(fMM.Lines.Text) + 1;
  fMM.Update;
end;

procedure TGuiConsole.CheckLimit(aLimit: TInteger);
Begin
  while fMM.Lines.Count > aLimit do fMM.Lines.Delete(0);
End;

procedure TGuiConsole.Clear;
begin
  fMM.lines.clear;
end;

procedure TGuiConsole.AppendStdOut(msg: TString);
begin
  fMM.lines.BeginUpdate;
  fMM.lines.append(msg);
  fMM.lines.EndUpdate;
  fMM.SelStart := Length(fMM.Lines.Text);
end;

{$endif}

{$ifndef lcl}
constructor TTextConsole.create;
begin
  inherited create;
  fStdErrLineCount:=0;
  fStdOutLineCount:=0;
end;

destructor TTextConsole.destroy;
begin
  inherited destroy;
end;

procedure TTextConsole.Clear;
begin
  fStdErrLineCount:=0;
  fStdOutLineCount:=0;
  AppendStdOut('----------- CLEAR -------------');
end;

procedure TTextConsole.AppendStdErr(msg: TString);
begin
  writeln(StdErr, msg);
  inc(fStdErrLineCount);
end;

procedure TTextConsole.AppendStdOut(msg: TString);
begin
  writeln(StdOut, msg);
  inc(fStdOutLineCount);
end;
procedure TTextConsole.CheckLimit(aLimit: TInteger);
Begin
  if (fStdOutLineCount>aLimit) then begin
    //do Somthing
  end;
End;
{$endif}

{$ifdef lcl}
constructor TLogger.Create (aIoClass: TMemo; useExcept: TBoolean);
begin
  fMmPort := TGuiConsole.create(aIoClass);
  fPrompt := '';
  fUseException := useExcept;
  SetLineLimit(MM_OUT_MAX_LINE);
  SetUseErrorDialog(TRUE);
end;
{$else}
constructor TLogger.Create;
begin
  fMmPort := TTextConsole.create;
  fUseException := DEF_USE_EXCEPTIONS;
  SetLineLimit(MM_OUT_MAX_LINE);
  SetUseErrorDialog(TRUE);
end;
{$endif}

destructor TLogger.Destroy;
begin
  if fMmPort <> NIL then FreeAndNil(fMmPort);
end;

procedure TLogger.OpenLog(aName:String);
begin
  {$I+}
  AssignFile(fText,aName);
  rewrite(fText);
  fLog:=true;
end;

procedure TLogger.CloseLog;
begin
  if not fLog then exit;
  close(fText)
end;

function TLogger.GetLineLimit: TInteger;
begin
  Result := fLineLimit;
end;

procedure TLogger.SetLineLimit (aLmt: TInteger);
begin
  if aLmt < 0 then aLmt := -1;
  if (aLmt <> -1) and (aLmt < MM_OUT_MIN_LINE) then aLmt := MM_OUT_MIN_LINE;
  if aLmt > MM_OUT_MAX_LINE then aLmt := MM_OUT_MAX_LINE;
  if aLmt <> fLineLimit then begin
    fLineLimit := aLmt;
    fMmPort.CheckLimit(fLineLimit);
  end;
end;

procedure TLogger.setDebugLevel (aLvl: TInteger);
begin
  if (aLvl < 0) then aLvl := 0; // No Debugging
  fDebug := aLvl;
end;

function TLogger.GetDebugLevel: TInteger;
begin
  Result := fDebug;
end;

function TLogger.GetErrorDialogUsage: TBoolean;
begin
  Result := fUseErrorDialog;
end;

procedure TLogger.SetUseErrorDialog (aUseDlg: TBoolean);
begin
  fUseErrorDialog := aUseDlg;
end;

procedure TLogger.CheckLineLimit;
begin
  fMmPort.CheckLimit(fLineLimit);
end;

procedure TLogger.Clear;
begin
    fMmPort.Clear;
end;

procedure TLogger.InfoPopup(aMsg:TString);
begin
  {$ifdef lcl}
     MessageDlg(aMsg, mtInformation, [mbOK], 0);
  {$else}
     Write(aMsg,' [PRESS-ENTER]');
     readln;
  {$endif}
end;

procedure TLogger.SelectOut (aMsg: TString; aChn: TIOChannel);
begin

  case aChn of
    IO_ECHO:  aMsg := FormatDateTime('hh:mm:ss.zzz',Now)+': ' + aMsg;
    IO_OUT:   aMsg := MM_OUT_OUT_PFX   + aMsg;
    IO_INFO:  aMsg := MM_OUT_INFO_PFX + FormatDateTime('hh:mm:ss.zzz',Now)+': ' + aMsg;
    IO_WARN:  aMsg := MM_OUT_WARN_PFX  + aMsg;
    IO_ERROR: aMsg := MM_OUT_ERROR_PFX + aMsg;
    IO_DEBUG: aMsg := MM_OUT_DEBUG_PFX + aMsg;
    else
     aMsg:= '????:'+aMsg;
  end;

  {$ifdef lcl}
    if fUseErrorDialog and (aChn = IO_ERROR)
         then begin
           MessageDlg(aMsg, mtError, [mbOK], 0);
         end;
  {$endif}
  CheckLineLimit;
  if fLog then begin
    writeln(fText, aMsg);
    Flush(fText);
  end;
  if aChn in [IO_ERROR, IO_DEBUG] then
    fMmPort.appendStdErr(aMsg)
  else
    fMmPort.AppendStdOut(aMsg);

  if fUseException and (aChn = IO_ERROR) then
       raise Exception.Create(aMsg);
end;

procedure TLogger.ErrorFmt (aFmt: TString; aArgs: array of const);
begin
  SelectOut(Format(aFmt, aArgs), IO_ERROR);
end;

procedure TLogger.ErrorLn (aMsg: TString);
begin
  SelectOut(aMsg, IO_ERROR);
end;

procedure TLogger.ErrorLn (aMsgs: array of TString);

var
  lSum: TString;
  ix : Tinteger;
begin
  lSum := '';
  for ix := Low(aMsgs) to High(aMsgs) do begin
    if ix>Low(aMsgs) then lSum+= LineEnding;
    lSum += aMsgs[ix];
  end;
   SelectOut(lSum, IO_ERROR);
end;


procedure TLogger.DebugFmt (aLvl: TInteger; aFmt: TString; aArgs: array of const);
begin
  if fDebug < aLvl then exit;
  SelectOut(Format(aFmt, aArgs), IO_DEBUG);
end;

procedure TLogger.DebugLn (aLvl: TInteger; aMsg: TString);
begin
  if fDebug < aLvl then exit;
  SelectOut(aMsg, IO_DEBUG);
end;

procedure TLogger.DebugLn (aLvl: TInteger; aMsgs: TStrings);
var
  ix: integer;
begin
  if fDebug < aLvl then exit;
  for ix := 0 to aMsgs.Count - 1 do
    SelectOut(Format('%03d: %s', [ix + 1, aMsgs[ix]]), IO_DEBUG);
end;

procedure TLogger.OutLn ( aMsgs: TStrings);
var
  ix: integer;
begin
  for ix := 0 to aMsgs.Count - 1 do
    SelectOut(Format('%03d: %s', [ix + 1, aMsgs[ix]]), IO_OUT);
end;

procedure TLogger.OutLN (aMsg: TString);
begin
  SelectOut(aMsg, IO_OUT);
end;

procedure TLogger.OutFmt (aFmt: TString; aArgs: array of const);
begin
  SelectOut(Format(aFmt, aArgs), IO_OUT);
end;

procedure TLogger.InfoLn ( aMsgs: TStrings);
var
  ix: integer;
begin
  for ix := 0 to aMsgs.Count - 1 do
    SelectOut(Format('%03d: %s', [ix + 1, aMsgs[ix]]), IO_Info);
end;

procedure TLogger.InfoLN (aMsg: TString);
begin
  SelectOut(aMsg, IO_Info);
end;

procedure TLogger.InfoFmt (aFmt: TString; aArgs: array of const);
begin
  SelectOut(Format(aFmt, aArgs), IO_Info);
end;

procedure TLogger.Echo (aMsg: TString; useDlg: TBoolean);
begin
  SelectOut(aMsg, IO_ECHO);
  {$ifdef lcl}
    if useDlg
         then begin
           MessageDlg(aMsg, mtInformation, [mbOK], 0);
         end;
  {$endif}

end;

procedure TLogger.EchoFmt (aFmt: TString; aArgs: array of const; useDlg: TBoolean);
var
  aMsg: String;
begin
  aMsg := Format(aFmt, aArgs);
  SelectOut( aMsg, IO_ECHO);
  {$ifdef lcl}
    if useDlg
         then begin
           MessageDlg(aMsg, mtInformation, [mbOK], 0);
         end;
  {$endif}
end;

end.
