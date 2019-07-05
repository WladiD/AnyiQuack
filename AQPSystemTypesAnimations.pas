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
  Winapi.Windows,
  System.SysUtils,
  System.Types,
  System.UITypes,

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

    function ColorAnimation(const TargetColor: TColor;
      const Getter: TRefSystemTypeGetterFunction<TColor>;
      const Setter: TRefSystemTypeSetterProcedure<TColor>; Duration: Integer; ID: Integer = 0;
      const EaseFunction: TEaseFunction = nil; const OnComplete: TAnonymNotifyEvent = nil): TAQ;

    function RectAnimation(TargetRect: TRect;
      const Getter: TRefSystemTypeGetterFunction<TRect>;
      const Setter: TRefSystemTypeSetterProcedure<TRect>; Duration: Integer; ID: Integer = 0;
      const EaseFunction: TEaseFunction = nil; const OnComplete: TAnonymNotifyEvent = nil): TAQ;
  end;

implementation

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

function TAQPSystemTypesAnimations.ColorAnimation(const TargetColor: TColor;
  const Getter: TRefSystemTypeGetterFunction<TColor>;
  const Setter: TRefSystemTypeSetterProcedure<TColor>; Duration, ID: Integer;
  const EaseFunction: TEaseFunction; const OnComplete: TAnonymNotifyEvent): TAQ;
begin
  Result := Each(
    function(AQ: TAQ; O: TObject): Boolean
    var
      FromColor: TColor;
    begin
      Result := True;

      FromColor := Getter(O);

      Take(O).EachAnimation(Duration,
        function(AQ: TAQ; O: TObject): Boolean
        var
          Progress: Real;
          AniColor: TColor;
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

function TAQPSystemTypesAnimations.RectAnimation(TargetRect: TRect;
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
          AniRect := TAQ.EaseRect(FromRect, TargetRect, Progress, EaseFunction);

          Setter(O, AniRect);

          if Progress = 1 then
            FireCompleteEvent(O, OnComplete{$IFDEF OutputDebugAnimation}, 'RectAnimation'{$ENDIF});
        end, nil, ID);
    end);
end;

end.
