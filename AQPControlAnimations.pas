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

{$INCLUDE Compile.inc}

uses
  System.Math,
  System.SysUtils,
  System.UITypes,
  System.Classes,
{$IFDEF FMX}
  FMX.Controls,
  FMX.Forms,
  FMX.Objects,
  FMX.Graphics,
{$ELSE}
  Vcl.Controls,
  Vcl.Forms,
  Vcl.Graphics,
{$ENDIF}
  AnyiQuack;

type
  TAQPControlAnimations = class(TAQPlugin)
  protected
    function Swing(Times, Diff: Integer; Progress: Real): Integer;
    procedure CustomColorAnimation(FromColor, ToColor: {$IFDEF FMX}TAlphaColor{$ELSE}TColor{$ENDIF};
      Duration: Integer; ID: Integer;
      const ColorAssignFunction: TEachMiscFunction<{$IFDEF FMX}TAlphaColor{$ELSE}TColor{$ENDIF}>;
      const EaseFunction: TEaseFunction = nil;
      const OnComplete: TAnonymNotifyEvent = nil);
  public
    function AlphaBlendAnimation(ToAlphaBlendValue: Byte; Duration: Integer; ID: Integer = 0;
      const EaseFunction: TEaseFunction = nil; const OnComplete: TAnonymNotifyEvent = nil): TAQ;
    function BoundsAnimation(NewLeft, NewTop, NewWidth, NewHeight: Integer; Duration: Integer;
      ID: Integer = 0; const EaseFunction: TEaseFunction = nil;
      const OnComplete: TAnonymNotifyEvent = nil): TAQ;
    function ShakeAnimation(XTimes, XDiff, YTimes, YDiff, Duration: Integer; ID: Integer = 0;
      const OnComplete: TAnonymNotifyEvent = nil): TAQ;
    {$IFNDEF FMX}
    function BackgroundColorAnimation(ToColor: TColor; Duration: Integer; ID: Integer = 0;
      const EaseFunction: TEaseFunction = nil; const OnComplete: TAnonymNotifyEvent = nil): TAQ;
    function FontColorAnimation(ToColor: TColor; Duration: Integer; ID: Integer = 0;
      const EaseFunction: TEaseFunction = nil; const OnComplete: TAnonymNotifyEvent = nil): TAQ;
    {$ELSE}
    function BackgroundColorAnimation<T: class>(Control: T; FromColor, ToColor: TAlphaColor;
      Duration: Integer; ID: Integer = 0; const EaseFunction: TEaseFunction = nil;
      const OnComplete: TAnonymNotifyEvent = nil): TAQ;
    function FontColorAnimation<T: class>(Control: T; FromColor, ToColor: TAlphaColor;
      Duration: Integer; ID: Integer = 0; const EaseFunction: TEaseFunction = nil;
      const OnComplete: TAnonymNotifyEvent = nil): TAQ;
    {$ENDIF}
  end;

implementation

type
{$IFNDEF FMX}
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
{$ENDIF}

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
  const EaseFunction: TEaseFunction; const OnComplete: TAnonymNotifyEvent): TAQ;
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

{$IFNDEF FMX}
function TAQPControlAnimations.BackgroundColorAnimation(ToColor: TColor; Duration, ID: Integer;
  const EaseFunction: TEaseFunction; const OnComplete: TAnonymNotifyEvent): TAQ;
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
{$ELSE}
function TAQPControlAnimations.BackgroundColorAnimation<T>(Control: T; 
  FromColor, ToColor: TAlphaColor; Duration: Integer; ID: Integer;
  const EaseFunction: TEaseFunction; const OnComplete: TAnonymNotifyEvent): TAQ;
begin
  Result := Each(
    function(AQ: TAQ; O: TObject): Boolean
    begin
      Result := True; // Komplett durchlaufen
      if O is TControl then
        Take(O)
          .Plugin<TAQPControlAnimations>
          .CustomColorAnimation(FromColor, ToColor, Duration, ID,
            function(AQ: TAQ; O: TObject; Color: TAlphaColor): Boolean
            begin
              if Control is TShape then
                (Control as TShape).Fill.Color := Color;
              Result := True;
            end,
            EaseFunction, OnComplete);
    end);
end;
{$ENDIF}

function TAQPControlAnimations.BoundsAnimation(
  NewLeft, NewTop, NewWidth, NewHeight, Duration, ID: Integer;
  const EaseFunction: TEaseFunction; const OnComplete: TAnonymNotifyEvent): TAQ;
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

    PrevLeft:= {$IFDEF FMX}Round(OC.Position.X){$ELSE}OC.Left{$ENDIF};
    PrevTop:= {$IFDEF FMX}Round(OC.Position.Y){$ELSE}OC.Top{$ENDIF};
    PrevWidth:= {$IFDEF FMX}Round(OC.Width){$ELSE}OC.Width{$ENDIF};
    PrevHeight:= {$IFDEF FMX}Round(OC.Height){$ELSE}OC.Height{$ENDIF};

    if not ((O is TControl) and
      ((NewLeft <> PrevLeft) or
        (NewTop <> PrevTop) or
          (NewWidth <> PrevWidth) or 
            (NewHeight <> PrevHeight))) then
      Exit;

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
        AniWidth := {$IFDEF FMX}Round({$ENDIF}OOC.Width{$IFDEF FMX}){$ENDIF};
      if NewHeight >= 0 then
        AniHeight := TAQ.EaseInteger(PrevHeight, NewHeight, Progress, EaseFunction)
      else
        AniHeight := {$IFDEF FMX}Round({$ENDIF}OOC.Height{$IFDEF FMX}){$ENDIF};

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

procedure TAQPControlAnimations.CustomColorAnimation(FromColor, ToColor: {$IFDEF FMX}TAlphaColor{$ELSE}TColor{$ENDIF};
  Duration, ID: Integer; const ColorAssignFunction: TEachMiscFunction<{$IFDEF FMX}TAlphaColor{$ELSE}TColor{$ENDIF}>;
  const EaseFunction: TEaseFunction; const OnComplete: TAnonymNotifyEvent);
begin
  if FromColor = ToColor then
    Exit;

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

{$IFNDEF FMX}
function TAQPControlAnimations.FontColorAnimation(ToColor: TColor; Duration, ID: Integer;
  const EaseFunction: TEaseFunction; const OnComplete: TAnonymNotifyEvent): TAQ;
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
{$ELSE}
function TAQPControlAnimations.FontColorAnimation<T>(Control: T; 
  FromColor, ToColor: TAlphaColor; Duration: Integer; ID: Integer;
  const EaseFunction: TEaseFunction; const OnComplete: TAnonymNotifyEvent): TAQ;
begin
  Result := Each(
    function(AQ: TAQ; O: TObject): Boolean
    begin
      Result := True; // Komplett durchlaufen
      if O is TTextControl then
        Take(O)
          .Plugin<TAQPControlAnimations>
          .CustomColorAnimation(FromColor, ToColor, Duration, ID,
            function(AQ: TAQ; O: TObject; Color: TAlphaColor): Boolean
            begin
              if Control is TTextControl  then
                (Control as TTextControl).FontColor := Color;
              Result := True;
            end,
            EaseFunction, OnComplete);
    end);
end;
{$ENDIF}

function TAQPControlAnimations.ShakeAnimation(XTimes, XDiff, YTimes, YDiff, Duration, ID: Integer;
  const OnComplete: TAnonymNotifyEvent): TAQ;
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

    PrevLeft:= {$IFDEF FMX}Round(OC.Position.X){$ELSE}OC.Left{$ENDIF};
    PrevTop:= {$IFDEF FMX}Round(OC.Position.Y){$ELSE}OC.Top{$ENDIF};
    
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

{$IFNDEF FMX}
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

{$ENDIF}

{** TFormRobin **}

function TFormRobin.GetAlphaBlendActivated: Boolean;
begin
  {$IFNDEF FMX}
  Result := AlphaBlend;
  {$ELSE}
  Result:= Transparency;
  {$ENDIF}
end;

function TFormRobin.GetAlphaBlendValue: Byte;
{$IFDEF FMX}
var
  ac: TAlphaColorRec;
{$ENDIF}
begin
  {$IFNDEF FMX}
  Result := inherited AlphaBlendValue;
  {$ELSE}
  ac:=TAlphaColorRec.Create(Fill.Color);
  Result := ac.A;
  {$ENDIF}
end;

procedure TFormRobin.SetAlphaBlendValue(NewAlphaBlendValue: Byte);
{$IFDEF FMX}
var
  ac: TAlphaColorRec;
{$ENDIF}
begin
  {$IFNDEF FMX}
  inherited AlphaBlendValue := NewAlphaBlendValue;
  {$ELSE}
  if NewAlphaBlendValue = $FF then
    Self.Transparency:=False
  else
  begin
    ac:=TAlphaColorRec.Create(Fill.Color);
    ac.A:=NewAlphaBlendValue;
    inherited Fill.Color := ac.Color;
  end;
  {$ENDIF}
end;

end.
