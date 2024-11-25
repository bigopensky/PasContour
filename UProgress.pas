unit UProgress;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, UDef;

Type

  TProgress = class(TObject)
    private
      fNumReady: TInteger;

      fNumSteps: TInteger;

      fCurTime: TTimeStamp;

      fLastTime: TTimeStamp;

      fPollTime: TInteger;

      fStartTime: TTimeStamp;

      { Event to trigger time statisiscs }
      fOnProgress: TNotifyEvent;

     public

      Constructor Create;
      Destructor Destroy; override;
      procedure Clear;
      procedure Init(aNumSteps: TInteger);
      Procedure GetState(var oNumStep, oNumReady, oNumLeft: TInteger;
                        var oPerc, oRate: TReal;
                        var oMsELA,  oMsETA, oMsRUN: TMillies);
      procedure Update(aIncReady: TInteger; aIgnorePoll: TBoolean =  FALSE);
      property  OnUpdateTime: TNotifyEvent read fOnProgress write fOnProgress;

  end;

implementation

constructor TProgress.Create; begin
  inherited create;
  fPollTime:= 1000;
end;

destructor TProgress.Destroy;
begin
  fOnProgress:= NIL;
  inherited destroy;
end;

procedure TProgress.Init(aNumSteps: TInteger);
begin
  Clear;
  fNumSteps:=aNumSteps;
  Update(0, TRUE);
end;

procedure TProgress.Clear;
begin
  fCurTime   := DateTimeToTimeStamp(now);
  fLastTime  := fCurTime;
  fStartTime := fCurTime;
  fNumReady  := 0;
  fNumSteps  := 0;
end;

procedure TProgress.Update(aIncReady: TInteger; aIgnorePoll: TBoolean =  FALSE);
begin
  if aIncReady > 0  then inc(fNumReady, aIncReady);
  fCurTime  := DateTimeToTimeStamp(now);
  if aIgnorePoll or (TimeStampToMSecs(fCurTime)
     - TimeStampToMSecs(fLastTime) > fPollTime) then begin
    if Assigned(fOnProgress) then fOnProgress(Self);
    fLastTime := fCurTime;
  end;
end;

Procedure TProgress.GetState(var oNumStep, oNumReady, oNumLeft: TInteger;
                            var oPerc,   oRate: TReal;
                            var oMsELA,  oMsETA, oMsRUN: TMillies);
begin
  oNumStep   := fNumSteps;
  oNumReady := fNumReady;
  oNumLeft  := fNumSteps - fNumReady;
  oPerc     := fNumReady / fNumSteps;
  oMsELA    := TimeStampToMSecs(fCurTime)
             - TimeStampToMSecs(fStartTime);
  oMsETA    := round( (1 - oPerc) * oMsELA);
  oMsRun    := oMsEla + oMsEta;
  oRate     := oNumReady / oMsEla * 1000; // (steps per scond)
  oPerc     *= 100.0;
end;


end.

