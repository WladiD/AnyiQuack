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
 * The Original Code is AQPSystemTypesAnimations.pas.
 *
 * The Initial Developer of the Original Code is Waldemar Derr.
 * Portions created by Waldemar Derr are Copyright (C) Waldemar Derr.
 * All Rights Reserved.
 *
 * @author Waldemar Derr <furevest@gmail.com>
 *}

unit AQPSystemTypesAnimations;

interface

uses
  System.SysUtils,
  System.Types,
  System.UITypes,
  System.Math,

  AnyiQuack;

{$INCLUDE Compile.inc}

type
  TRefSystemTypeGetterFunction<T> = reference to function(RefObject: TObject): T;
  TRefSystemTypeSetterProcedure<T> = reference to procedure(RefObject: TObject; const NewValue: T);

  TAQPSystemTypesAnimations = class(TAQPlugin)
  public
    function IntegerAnimation(TargetValue: Integer;
      const Getter: TRefSystemTypeGetterFunction<Integer>;
      const Setter: TRefSystemTypeSetterProcedure<Integer>; Duration: Integer; ID: Integer = 0;
      const EaseFunction: TEaseFunction = nil; const OnComplete: TAnonymNotifyEvent = nil): TAQ;

    function SingleAnimation(TargetValue: Single;
      const Getter: TRefSystemTypeGetterFunction<Single>;
      const Setter: TRefSystemTypeSetterProcedure<Single>; Duration: Integer; ID: Integer = 0;
      const EaseFunction: TEaseFunction = nil; const OnComplete: TAnonymNotifyEvent = nil): TAQ;

    function ColorAnimation(const TargetColor: {$IFDEF FMX}TAlphaColor{$ELSE}TColor{$ENDIF};
      const Getter: TRefSystemTypeGetterFunction<{$IFDEF FMX}TAlphaColor{$ELSE}TColor{$ENDIF}>;
      const Setter: TRefSystemTypeSetterProcedure<{$IFDEF FMX}TAlphaColor{$ELSE}TColor{$ENDIF}>;
      Duration: Integer; ID: Integer = 0;
      const EaseFunction: TEaseFunction = nil; const OnComplete: TAnonymNotifyEvent = nil): TAQ;

    function BlinkColor(
      const Getter: TRefSystemTypeGetterFunction<{$IFDEF FMX}TAlphaColor{$ELSE}TColor{$ENDIF}>;
      const Setter: TRefSystemTypeSetterProcedure<{$IFDEF FMX}TAlphaColor{$ELSE}TColor{$ENDIF}>;
      ABlinkColor: {$IFDEF FMX}TAlphaColor{$ELSE}TColor{$ENDIF};
      Times, Duration: Integer; ID: Integer = 0;
      const EaseFunction: TEaseFunction = nil; const OnComplete: TAnonymNotifyEvent = nil): TAQ;

    function RectAnimation(const TargetRect: TRect;
      const Getter: TRefSystemTypeGetterFunction<TRect>;
      const Setter: TRefSystemTypeSetterProcedure<TRect>; Duration: Integer; ID: Integer = 0;
      const EaseFunction: TEaseFunction = nil;
      const OnComplete: TAnonymNotifyEvent = nil): TAQ; overload;
    function RectAnimation(const GetTargetRect: TFunc<TRect>;
      const Getter: TRefSystemTypeGetterFunction<TRect>;
      const Setter: TRefSystemTypeSetterProcedure<TRect>; Duration: Integer; ID: Integer = 0;
      const EaseFunction: TEaseFunction = nil;
      const OnComplete: TAnonymNotifyEvent = nil): TAQ; overload;

    function ShakePoint(
      const Getter: TRefSystemTypeGetterFunction<TPointF>;
      const Setter: TRefSystemTypeSetterProcedure<TPointF>;
      XTimes, XDiff, YTimes, YDiff, Duration, ID: Integer;
      const OnComplete: TAnonymNotifyEvent): TAQ;
  end;

implementation

function SwingSingle(Times, Diff: Integer; Progress: Real): Single;
begin
  Result := Diff * Sin(Progress * Times * Pi * 2);
end;

procedure FireCompleteEvent(ForObject: TObject;
  const OnComplete: TAnonymNotifyEvent
  {$IFDEF OutputDebugAnimation}; const AnimationCaption: string{$ENDIF}); inline;
begin
  {$IFDEF OutputDebugAnimation}
  OutputDebugString(PChar(AnimationCaption + ' complete for $' +
    IntToHex(Integer(ForObject), SizeOf(Integer) * 2)));
  {$ENDIF}

  if Assigned(OnComplete) then
    OnComplete(ForObject);
end;

{ TAQPSystemTypesAnimations }

function TAQPSystemTypesAnimations.IntegerAnimation(TargetValue: Integer;
  const Getter: TRefSystemTypeGetterFunction<Integer>;
  const Setter: TRefSystemTypeSetterProcedure<Integer>; Duration, ID: Integer;
  const EaseFunction: TEaseFunction; const OnComplete: TAnonymNotifyEvent): TAQ;
begin
  Result := Each(
    function(AQ: TAQ; O: TObject): Boolean
    var
      FromValue: Integer;
    begin
      Result := True;

      FromValue := Getter(O);

      Take(O).EachAnimation(Duration,
        function(AQ: TAQ; O: TObject): Boolean
        var
          Progress: Real;
          AniValue: Integer;
        begin
          Result := True;
          Progress := AQ.CurrentInterval.Progress;
          AniValue := TAQ.EaseInteger(FromValue, TargetValue, Progress, EaseFunction);

          Setter(O, AniValue);

          if Progress = 1 then
            FireCompleteEvent(O, OnComplete{$IFDEF OutputDebugAnimation}, 'IntegerAnimation'{$ENDIF});
        end, nil, ID);
    end);
end;

function TAQPSystemTypesAnimations.SingleAnimation(TargetValue: Single;
  const Getter: TRefSystemTypeGetterFunction<Single>;
  const Setter: TRefSystemTypeSetterProcedure<Single>; Duration: Integer; ID: Integer = 0;
  const EaseFunction: TEaseFunction = nil; const OnComplete: TAnonymNotifyEvent = nil): TAQ;
begin
  Result := Each(
    function(AQ: TAQ; O: TObject): Boolean
    var
      FromValue: Single;
    begin
      Result := True;

      FromValue := Getter(O);

      Take(O).EachAnimation(Duration,
        function(AQ: TAQ; O: TObject): Boolean
        var
          Progress, AniValue: Real;
        begin
          Result := True;
          Progress := AQ.CurrentInterval.Progress;
          AniValue := TAQ.EaseReal(FromValue, TargetValue, Progress, EaseFunction);

          Setter(O, AniValue);

          if Progress = 1 then
            FireCompleteEvent(O, OnComplete{$IFDEF OutputDebugAnimation}, 'SingleAnimation'{$ENDIF});
        end, nil, ID);
    end);
end;

function TAQPSystemTypesAnimations.ColorAnimation(
  const TargetColor: {$IFDEF FMX}TAlphaColor{$ELSE}TColor{$ENDIF};
  const Getter: TRefSystemTypeGetterFunction<{$IFDEF FMX}TAlphaColor{$ELSE}TColor{$ENDIF}>;
  const Setter: TRefSystemTypeSetterProcedure<{$IFDEF FMX}TAlphaColor{$ELSE}TColor{$ENDIF}>; Duration, ID: Integer;
  const EaseFunction: TEaseFunction; const OnComplete: TAnonymNotifyEvent): TAQ;
begin
  Result := Each(
    function(AQ: TAQ; O: TObject): Boolean
    var
      FromColor: {$IFDEF FMX}TAlphaColor{$ELSE}TColor{$ENDIF};
    begin
      Result := True;

      FromColor := Getter(O);

      Take(O).EachAnimation(Duration,
        function(AQ: TAQ; O: TObject): Boolean
        var
          Progress: Real;
          AniColor: {$IFDEF FMX}TAlphaColor{$ELSE}TColor{$ENDIF};
        begin
          Result := True;
          Progress := AQ.CurrentInterval.Progress;
          AniColor := TAQ.EaseColor(FromColor, TargetColor, Progress, EaseFunction);

          Setter(O, AniColor);

          if Progress = 1 then
            FireCompleteEvent(O, OnComplete{$IFDEF OutputDebugAnimation}, 'ColorAnimation'{$ENDIF});
        end, nil, ID);
    end);
end;

function TAQPSystemTypesAnimations.BlinkColor(
  const Getter: TRefSystemTypeGetterFunction<{$IFDEF FMX}TAlphaColor{$ELSE}TColor{$ENDIF}>;
  const Setter: TRefSystemTypeSetterProcedure<{$IFDEF FMX}TAlphaColor{$ELSE}TColor{$ENDIF}>;
  ABlinkColor: {$IFDEF FMX}TAlphaColor{$ELSE}TColor{$ENDIF};
  Times, Duration, ID: Integer;
  const EaseFunction: TEaseFunction; const OnComplete: TAnonymNotifyEvent): TAQ;
begin
  Result := Each(
    function(AQ: TAQ; O: TObject): Boolean
    var
      FromColor: {$IFDEF FMX}TAlphaColor{$ELSE}TColor{$ENDIF};
      Colors: TArray<{$IFDEF FMX}TAlphaColor{$ELSE}TColor{$ENDIF}>;
      cc: Integer;
    begin
      Result := True;

      FromColor := Getter(O);
      SetLength(Colors, 1 + (Times * 2));
      Colors[0] := FromColor;
      for cc := 0 to Times - 1 do
      begin
        Colors[1 + (cc * 2)] := ABlinkColor;
        Colors[2 + (cc * 2)] := FromColor;
      end;

      Take(O).EachAnimation(Duration,
        function(AQ: TAQ; O: TObject): Boolean
        begin
          Result := True;
          Setter(O, TAQ.EaseColor(Colors, AQ.CurrentInterval.Progress, EaseFunction));

          if AQ.CurrentInterval.Progress = 1 then
            FireCompleteEvent(O, OnComplete{$IFDEF OutputDebugAnimation}, 'BlinkColor'{$ENDIF});
        end, nil, ID);
    end);
end;

function TAQPSystemTypesAnimations.RectAnimation(const TargetRect: TRect;
  const Getter: TRefSystemTypeGetterFunction<TRect>;
  const Setter: TRefSystemTypeSetterProcedure<TRect>; Duration, ID: Integer;
  const EaseFunction: TEaseFunction; const OnComplete: TAnonymNotifyEvent): TAQ;
begin
  Result := RectAnimation(
    function: TRect
    begin
      Result := TargetRect;
    end,
    Getter, Setter, Duration, ID, EaseFunction, OnComplete);
end;

// This is a overloaded version for moving targets. The target rect is determined by the
// passed GetTargetRect lambda instead of a fixed rect.
function TAQPSystemTypesAnimations.RectAnimation(const GetTargetRect: TFunc<TRect>;
  const Getter: TRefSystemTypeGetterFunction<TRect>;
  const Setter: TRefSystemTypeSetterProcedure<TRect>; Duration, ID: Integer;
  const EaseFunction: TEaseFunction; const OnComplete: TAnonymNotifyEvent): TAQ;
begin
  Result := Each(
    function(AQ: TAQ; O: TObject): Boolean
    var
      FromRect: TRect;
    begin
      Result := True;

      FromRect := Getter(O);

      Take(O).EachAnimation(Duration,
        function(AQ: TAQ; O: TObject): Boolean
        var
          Progress: Real;
          AniRect: TRect;
        begin
          Result := True;
          Progress := AQ.CurrentInterval.Progress;
          AniRect := TAQ.EaseRect(FromRect, GetTargetRect, Progress, EaseFunction);

          Setter(O, AniRect);

          if Progress = 1 then
            FireCompleteEvent(O, OnComplete{$IFDEF OutputDebugAnimation}, 'RectAnimation'{$ENDIF});
        end, nil, ID);
    end);
end;

function TAQPSystemTypesAnimations.ShakePoint(const Getter: TRefSystemTypeGetterFunction<TPointF>;
  const Setter: TRefSystemTypeSetterProcedure<TPointF>; XTimes, XDiff, YTimes, YDiff, Duration,
  ID: Integer; const OnComplete: TAnonymNotifyEvent): TAQ;
begin
  Result := Each(
    function(AQ: TAQ; O: TObject): Boolean
    var
      PrevPoint: TPointF;
    begin
      Result := True;
      PrevPoint := Getter(O);

      Take(O).EachAnimation(Duration,
        function(AQ: TAQ; O: TObject): Boolean
        var
          Progress: Real;
          AniPoint: TPointF;
        begin
          Result := True;
          Progress := AQ.CurrentInterval.Progress;
          AniPoint := PrevPoint;

          if Progress < 1 then
          begin
            if XDiff > 0 then
              AniPoint.X := AniPoint.X + SwingSingle(XTimes, XDiff, Progress);
            if YDiff > 0 then
              AniPoint.Y := AniPoint.Y + SwingSingle(YTimes, YDiff, Progress);
          end;

          Setter(O, AniPoint);

          if Progress = 1 then
            FireCompleteEvent(O, OnComplete{$IFDEF OutputDebugAnimation}, 'ShakePoint'{$ENDIF});
        end, nil, ID);
    end);
end;

end.
