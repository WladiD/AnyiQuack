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
      ToolsForm: TToolsForm;
    begin
      Result := TRUE;
      if not (O is TToolsForm) then
        Exit;
      ToolsForm := TToolsForm(O);
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
              Result:=FALSE;
            end);
    end);
end;

function TAQPStickyTools.StickyEach(AQ: TAQ; O: TObject): Boolean;
var
  LocalTargetPos: TPoint;
  ToolsForm: TToolsForm;
begin
  Result := TRUE;
  if not (O is TToolsForm) then
    Exit;
  ToolsForm := TToolsForm(O);
  LocalTargetPos := ToolsForm.TargetPos;

  if AnimateStick then
  begin
    Take(O)
      .CancelAnimations
      .Plugin<TAQPControlAnimations>
      .BoundsAnimation(LocalTargetPos.X, LocalTargetPos.Y, -1, -1, 350, 0,
        TAQ.Ease(etSinus, emInSnakeInverted))
    .Die;
  end
  else
    with ToolsForm do
      SetBounds(LocalTargetPos.X, LocalTargetPos.Y, Width, Height);

end;

initialization
  TAQPStickyTools.AnimateStick := TRUE;

end.
