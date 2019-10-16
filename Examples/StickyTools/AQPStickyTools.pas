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
  private
    class var
    BoundsAnimationID: Integer;
    MessageListenID: Integer;
  protected
    procedure Autorun; override;
    function StickyEach(AQ: TAQ; O: TObject):Boolean;
  public
    class var
    AnimateStick: Boolean;

    class constructor Create;
  end;

implementation

uses
  Tools;

{ TAQPStickyTools }

class constructor TAQPStickyTools.Create;
begin
  BoundsAnimationID := TAQ.GetUniqueID;
  MessageListenID := TAQ.GetUniqueID;
end;

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
        .ListenersExistsFor(TControl(ToolsForm.Owner), WM_WINDOWPOSCHANGED, MessageListenID) then
        Take(ToolsForm.Owner)
          .Plugin<TAQPMessages>
          .EachMessage(WM_WINDOWPOSCHANGED,
            function(AQ: TAQ; O: TObject; Message: TMessage): Boolean
            begin
              AQ.ChildrenChain //.DebugMessage('Nach ChildrenChain')
                  .FilterChain(TToolsForm) //.DebugMessage('Nach FilterChain')
                    .Each(StickyEach)
                  .EndChain.Die
                .EndChain.Die;

              Result := False;
            end, MessageListenID);
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
      .CancelAnimations(BoundsAnimationID)
      .Plugin<TAQPControlAnimations>
      .BoundsAnimation(LocalTargetPos.X, LocalTargetPos.Y, -1, -1, 350, BoundsAnimationID,
        TAQ.Ease(etElastic, emIn))
      .Die;
  end
  else
    with ToolsForm do
      SetBounds(LocalTargetPos.X, LocalTargetPos.Y, Width, Height);
end;

initialization
  TAQPStickyTools.AnimateStick := True;

end.
