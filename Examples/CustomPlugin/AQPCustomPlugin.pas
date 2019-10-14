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
    var
      OC: TControl absolute O;
    begin
      if (O is TControl) and OC.Visible then
        OC.Hide;
      Result := True;
    end);
end;

function TAQPCustomPlugin.Show:TAQ;
begin
  Result := Each(
    function(AQ: TAQ; O :TObject): Boolean
    var
      OC: TControl absolute O;
    begin
      if (O is TControl) and not OC.Visible then
        OC.Show;
      Result := True;
    end);
end;

end.