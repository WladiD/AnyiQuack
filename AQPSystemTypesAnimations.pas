unit AQPSystemTypesAnimations;

interface

uses
  System.SysUtils,
  System.Types,
  System.UITypes,

  AnyiQuack;

type
  TRefSystemTypeGetterFunction<T> = reference to function(RefObject: TObject): T;
  TRefSystemTypeSetterProcedure<T> = reference to procedure(RefObject: TObject; const NewValue: T);

  TAQPSystemTypesAnimations = class(TAQPlugin)
  public
    function ColorAnimation(const TargetColor: TColor;
      const Getter: TRefSystemTypeGetterFunction<TColor>;
      const Setter: TRefSystemTypeSetterProcedure<TColor>; Duration: Integer; ID: Integer = 0;
      const EaseFunction: TEaseFunction = nil; const OnComplete: TAnonymNotifyEvent = nil): TAQ;

    function RectAnimation(const TargetRect: TRect;
      const Getter: TRefSystemTypeGetterFunction<TRect>;
      const Setter: TRefSystemTypeSetterProcedure<TRect>; Duration: Integer; ID: Integer = 0;
      const EaseFunction: TEaseFunction = nil; const OnComplete: TAnonymNotifyEvent = nil): TAQ;


  end;

implementation

{ TAQPSystemTypesAnimations }

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
        end, nil, ID);
    end);
end;

function TAQPSystemTypesAnimations.RectAnimation(const TargetRect: TRect;
  const Getter: TRefSystemTypeGetterFunction<TRect>;
  const Setter: TRefSystemTypeSetterProcedure<TRect>; Duration, ID: Integer;
  const EaseFunction: TEaseFunction; const OnComplete: TAnonymNotifyEvent): TAQ;
begin
  // Not implemented yet...
end;

end.
