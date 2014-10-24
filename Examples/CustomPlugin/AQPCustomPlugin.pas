unit AQPCustomPlugin;

interface

uses
  Controls, AnyiQuack;

type
  TAQPCustomPlugin = class(TAQPlugin)
  public
    function Hide: TAQ;
    function Show: TAQ;
  end;

implementation

{** TCustomPlugin **}

function TAQPCustomPlugin.Hide: TAQ;
begin
  Result := Each(
    function(AQ: TAQ; O: TObject): Boolean
    begin
      if (O is TControl) and (TControl(O).Visible) then
        TControl(O).Hide;
      Result := TRUE;
    end);
end;

function TAQPCustomPlugin.Show:TAQ;
begin
  Result := Each(
    function(AQ: TAQ; O :TObject): Boolean
    begin
      if (O is TControl) and (not TControl(O).Visible) then
        TControl(O).Show;
      Result := TRUE;
    end);
end;

end.