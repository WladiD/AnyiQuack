{**
 * The contents of this file are subject to the Mozilla Public License
 * Version 1.1 (the "License"); you may not use this file except in
 * compliance with the License. You may obtain a copy of the License at
 * http://www.mozilla.org/MPL/
 *
 * Software distributed under the License is distributed on an "AS IS"
 * basis, WITHOUT WARRANTY OF ANY KIND, either express or implied. See the
 * License for the specific language governing rights and limitations
 * under the License.
 *
 * The Original Code is AQPControlAnimations.pas.
 *
 * The Initial Developer of the Original Code is Waldemar Derr.
 * Portions created by Waldemar Derr are Copyright (C) Waldemar Derr.
 * All Rights Reserved.
 *
 * @author Waldemar Derr <furevest@gmail.com>
 *}

unit AQPControlAnimations;

interface

uses
  Winapi.Windows,
  System.Math,
  System.SysUtils,
  Vcl.Controls,
  Vcl.Forms,
  Vcl.Graphics,

  AnyiQuack;

{$INCLUDE Compile.inc}

type
  TAQPControlAnimations = class(TAQPlugin)
  protected
    function Swing(Times, Diff: Integer; Progress: Real): Integer;
    procedure CustomColorAnimation(FromColor, ToColor: TColor; Duration: Integer; ID: Integer;
      ColorAssignFunction: TEachMiscFunction<TColor>; EaseFunction: TEaseFunction = nil;
      OnComplete: TAnonymNotifyEvent = nil);
  public
    function AlphaBlendAnimation(ToAlphaBlendValue: Byte; Duration: Integer; ID: Integer = 0;
      EaseFunction: TEaseFunction = nil; OnComplete: TAnonymNotifyEvent = nil): TAQ;
    function BoundsAnimation(NewLeft, NewTop, NewWidth, NewHeight: Integer; Duration: Integer;
      ID: Integer = 0; EaseFunction: TEaseFunction = nil;
      OnComplete: TAnonymNotifyEvent = nil): TAQ;
    function ShakeAnimation(XTimes, XDiff, YTimes, YDiff, Duration: Integer; ID: Integer = 0;
      OnComplete: TAnonymNotifyEvent = nil): TAQ;
    function BackgroundColorAnimation(ToColor: TColor; Duration: Integer; ID: Integer = 0;
      EaseFunction: TEaseFunction = nil; OnComplete: TAnonymNotifyEvent = nil): TAQ;
    function FontColorAnimation(ToColor: TColor; Duration: Integer; ID: Integer = 0;
      EaseFunction: TEaseFunction = nil; OnComplete: TAnonymNotifyEvent = nil): TAQ;
  end;

implementation

type
  TControlRobin = class helper for TControl
  protected
    function GetBackgroundColor: TColor;
    procedure SetBackgroundColor(NewColor: TColor);
    function GetFontColor: TColor;
    procedure SetFontColor(NewColor: TColor);
  public
    property BackgroundColor: TColor read GetBackgroundColor write SetBackgroundColor;
    property FontColor: TColor read GetFontColor write SetFontColor;
  end;

  TFormRobin = class helper for TCustomForm
  protected
    function GetAlphaBlendActivated: Boolean;
    function GetAlphaBlendValue: Byte;
    procedure SetAlphaBlendValue(NewAlphaBlendValue: Byte);
  public
    property AlphaBlendActivated: Boolean read GetAlphaBlendActivated;
    property AlphaBlendValue: Byte read GetAlphaBlendValue write SetAlphaBlendValue;
  end;

{ TAQPControlAnimations }

{**
 * Animates the AlphaBlendValue on all contained TCustomForm descendants
 *
 * TCustomForm.AlphaBlend must be set to True previously.
 *}
function TAQPControlAnimations.AlphaBlendAnimation(ToAlphaBlendValue: Byte; Duration, ID: Integer;
  EaseFunction: TEaseFunction; OnComplete: TAnonymNotifyEvent): TAQ;
begin
  Result := Each(
    function(AQ: TAQ; O: TObject): Boolean
    var
      StartAlphaBlendValue: Byte;
    begin
      Result := True;

      if not (
        (O is TCustomForm) and
        TCustomForm(O).AlphaBlendActivated and
        (TCustomForm(O).AlphaBlendValue <> ToAlphaBlendValue)) then
        Exit;

      StartAlphaBlendValue := TCustomForm(O).AlphaBlendValue;

      Take(O)
        .EachAnimation(Duration,
          function(AQ: TAQ; O: TObject): Boolean
          begin
            TCustomForm(O).AlphaBlendValue := Byte(TAQ.EaseInteger(
              StartAlphaBlendValue, ToAlphaBlendValue, AQ.CurrentInterval.Progress,
              EaseFunction));

            if Assigned(OnComplete) and (AQ.CurrentInterval.Progress = 1) then
              OnComplete(O);

            Result := True;
          end);
    end);
end;

function TAQPControlAnimations.BackgroundColorAnimation(ToColor: TColor; Duration, ID: Integer;
  EaseFunction: TEaseFunction; OnComplete: TAnonymNotifyEvent): TAQ;
begin
  Result := Each(
    function(AQ: TAQ; O: TObject): Boolean
    begin
      Result := True; // Komplett durchlaufen
      if O is TControl then
        Take(O)
          .Plugin<TAQPControlAnimations>
          .CustomColorAnimation(TControl(O).BackgroundColor, ToColor, Duration, ID,
            function(AQ: TAQ; O: TObject; Color: TColor): Boolean
            begin
              TControl(O).BackgroundColor := Color;
              Result := True;
            end,
            EaseFunction, OnComplete);
    end);
end;

function TAQPControlAnimations.BoundsAnimation(
  NewLeft, NewTop, NewWidth, NewHeight, Duration, ID: Integer;
  EaseFunction: TEaseFunction; OnComplete: TAnonymNotifyEvent): TAQ;
var
  WholeEach: TEachFunction;
begin
  WholeEach := function(AQ: TAQ; O: TObject): Boolean
  var
    EachF: TEachFunction;
    PrevLeft, PrevTop, PrevWidth, PrevHeight: Integer;
    OC: TControl absolute O;
  begin
    Result := True;
    if not ((O is TControl) and
      ((NewLeft <> OC.Left) or (NewTop <> OC.Top) or
      (NewWidth <> OC.Width) or (NewHeight <> OC.Height))) then
      Exit;

    PrevLeft := OC.Left;
    PrevTop := OC.Top;
    PrevWidth := OC.Width;
    PrevHeight := OC.Height;

    EachF := function(AQ: TAQ; O: TObject): Boolean
    var
      Progress: Real;
      AniLeft, AniTop, AniWidth, AniHeight: Integer;
      OOC: TControl absolute O;
    begin
      Result := True;
      Progress := AQ.CurrentInterval.Progress;

      AniLeft := TAQ.EaseInteger(PrevLeft, NewLeft, Progress, EaseFunction);
      AniTop := TAQ.EaseInteger(PrevTop, NewTop, Progress, EaseFunction);
      if NewWidth >= 0 then
        AniWidth := TAQ.EaseInteger(PrevWidth, NewWidth, Progress, EaseFunction)
      else
        AniWidth := OOC.Width;
      if NewHeight >= 0 then
        AniHeight := TAQ.EaseInteger(PrevHeight, NewHeight, Progress, EaseFunction)
      else
        AniHeight := OOC.Height;

      OOC.SetBounds(AniLeft, AniTop, AniWidth, AniHeight);

      if Progress = 1 then
      begin
        {$IFDEF OutputDebugAnimation}
        OutputDebugString(PWideChar('BoundsAnimation beendet für $' +
          IntToHex(Integer(O), SizeOf(Integer) * 2)));
        {$ENDIF}

        if Assigned(OnComplete) then
          OnComplete(O);
      end;
    end;

    Take(O).EachAnimation(Duration, EachF, nil, ID);
  end;

  Result := Each(WholeEach);
end;

procedure TAQPControlAnimations.CustomColorAnimation(FromColor, ToColor: TColor;
  Duration, ID: Integer; ColorAssignFunction: TEachMiscFunction<TColor>;
  EaseFunction: TEaseFunction; OnComplete: TAnonymNotifyEvent);
begin
  if FromColor = ToColor then
    Exit;

  EaseFunction := (TAQ.Ease(EaseFunction));
  WorkAQ.EachAnimation(Duration,
    function(AQ: TAQ; O: TObject): Boolean
    var
      Progress: Real;
    begin
      Progress := AQ.CurrentInterval.Progress;
      Result := ColorAssignFunction(AQ, O,
        TAQ.EaseColor(FromColor, ToColor, Progress, EaseFunction));

      if (Progress = 1) and Assigned(OnComplete) then
        OnComplete(O);
    end,
    nil, ID);
end;

function TAQPControlAnimations.FontColorAnimation(ToColor: TColor; Duration, ID: Integer;
  EaseFunction: TEaseFunction; OnComplete: TAnonymNotifyEvent): TAQ;
begin
  Result := Each(
    function(AQ: TAQ; O: TObject): Boolean
    begin
      Result := True; // Komplett durchlaufen
      if O is TControl then
        Take(O)
          .Plugin<TAQPControlAnimations>
          .CustomColorAnimation(TControl(O).FontColor, ToColor, Duration, ID,
            function(AQ: TAQ; O: TObject; Color: TColor): Boolean
            begin
              TControl(O).FontColor := Color;
              Result := True;
            end,
            EaseFunction, OnComplete);
    end);
end;

function TAQPControlAnimations.ShakeAnimation(XTimes, XDiff, YTimes, YDiff, Duration, ID: Integer;
  OnComplete: TAnonymNotifyEvent): TAQ;
var
  WholeEach: TEachFunction;
begin
  WholeEach := function(AQ: TAQ; O: TObject): Boolean
  var
    EachF: TEachFunction;
    PrevLeft, PrevTop: Integer;
    OC: TControl absolute O;
  begin
    Result := True;
    if not (O is TControl) then
      Exit;

    PrevLeft := OC.Left;
    PrevTop := OC.Top;

    EachF := function(AQ: TAQ; O: TObject): Boolean
    var
      Progress: Real;
      AniLeft, AniTop: Integer;
      OC: TControl absolute O;
    begin
      Result := True;
      Progress := AQ.CurrentInterval.Progress;
      AniLeft := PrevLeft;
      AniTop := PrevTop;

      if Progress < 1 then
      begin
        if XDiff > 0 then
          AniLeft := AniLeft + Swing(XTimes, XDiff, Progress);
        if YDiff > 0 then
          AniTop := PrevTop + Swing(YTimes, YDiff, Progress);
      end;

      OC.SetBounds(AniLeft, AniTop, OC.Width, OC.Height);

      if Progress = 1 then
      begin
        {$IFDEF OutputDebugAnimation}
        OutputDebugString(PWideChar('ShakeAnimation beendet für $' + IntToHex(Integer(O),
          SizeOf(Integer) * 2)));
        {$ENDIF}

        if Assigned(OnComplete) then
          OnComplete(O);
      end;
    end;

    Take(O).EachAnimation(Duration, EachF, nil, ID);
  end;

  Result := Each(WholeEach);
end;

function TAQPControlAnimations.Swing(Times, Diff: Integer; Progress: Real): Integer;
begin
  Result := Ceil(Diff * Sin(Progress * Times * Pi * 2));
end;

{** TControlRobin **}

function TControlRobin.GetBackgroundColor: TColor;
begin
  Result := Color;
end;

function TControlRobin.GetFontColor: TColor;
begin
  Result := Font.Color;
end;

procedure TControlRobin.SetBackgroundColor(NewColor: TColor);
begin
  Color := NewColor;
end;

procedure TControlRobin.SetFontColor(NewColor: TColor);
begin
  Font.Color := NewColor;
end;

{** TFormRobin **}

function TFormRobin.GetAlphaBlendActivated: Boolean;
begin
  Result := AlphaBlend;
end;

function TFormRobin.GetAlphaBlendValue: Byte;
begin
  Result := inherited AlphaBlendValue;
end;

procedure TFormRobin.SetAlphaBlendValue(NewAlphaBlendValue: Byte);
begin
  inherited AlphaBlendValue := NewAlphaBlendValue;
end;

end.
