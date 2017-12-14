unit AQPStickyTools;

interface

uses
  Controls, Windows, Messages, Contnrs,
  AnyiQuack,
  {**
   * Verwendete Plugins
   *}
  AQPControlAnimations, AQPMessages;

type
  TAQPStickyTools = class(TAQPlugin)
  protected
    procedure Autorun; override;
    function StickyEach(AQ: TAQ; O: TObject):Boolean;
  public
    class var AnimateStick: Boolean;
  end;

implementation

uses
  Tools;

{** TAQPStickyTools **}

procedure TAQPStickyTools.Autorun;
begin
  inherited Autorun;

  Each(
    function(AQ: TAQ; O: TObject): Boolean
    var
      ToolsForm: TToolsForm absolute O;
    begin
      Result := True;
      if not (O is TToolsForm) then
        Exit;
      if not TAQPMessages
        .ListenersExistsFor(TControl(ToolsForm.Owner), WM_WINDOWPOSCHANGED) then
        Take(ToolsForm.Owner)
          .Plugin<TAQPMessages>
          .EachMessage(WM_WINDOWPOSCHANGED,
            function(AQ: TAQ; O: TObject; Message: TMessage): Boolean
            begin
              AQ
                .ChildrenChain
                  .Each(StickyEach)
                  .Die
                .EndChain;
              Result := False;
            end);
    end);
end;

function TAQPStickyTools.StickyEach(AQ: TAQ; O: TObject): Boolean;
var
  LocalTargetPos: TPoint;
  ToolsForm: TToolsForm absolute O;
begin
  Result := True;
  if not (O is TToolsForm) then
    Exit;
  LocalTargetPos := ToolsForm.TargetPos;

  if AnimateStick then
  begin
    Take(O)
      .CancelAnimations(161) // 161 is an random number but with the purpose to associate this animation
      .Plugin<TAQPControlAnimations>
      .BoundsAnimation(LocalTargetPos.X, LocalTargetPos.Y, -1, -1, 350, 161,
        TAQ.Ease(etElastic, emIn))
    .Die;
  end
  else
    with ToolsForm do
      SetBounds(LocalTargetPos.X, LocalTargetPos.Y, Width, Height);
end;

initialization
  TAQPStickyTools.AnimateStick := TRUE;

end.
