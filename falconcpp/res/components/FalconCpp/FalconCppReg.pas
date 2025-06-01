unit FalconCppReg;

interface

uses
  Classes, DesignEditors, DesignIntf, ModernTabs, XPPanels;

type
  TModernPageControlEditor = class(TComponentEditor)
  public
    procedure ExecuteVerb(Index: Integer); override;
    function GetVerb(Index: Integer): string; override;
    function GetVerbCount: Integer; override;
  end;

procedure Register;

implementation

uses SysUtils;

procedure Register;
begin
  RegisterComponents('Falcon C++', [TXPPanel, TSplitterPanel,
    TModernPageControl]);
  RegisterComponentEditor(TModernPageControl, TModernPageControlEditor);
  RegisterComponentEditor(TModernTabSheet, TModernPageControlEditor);
  RegisterNoIcon([TModernTabSheet]);
  RegisterClasses([TModernTabSheet]);
end;


{ TModernPageControlEditor }

procedure TModernPageControlEditor.ExecuteVerb(Index: Integer);
var
  PageControl: TModernPageControl;
  TabSheet: TModernTabSheet;
begin
  if GetComponent is TModernTabSheet then
    PageControl := TModernPageControl(TModernTabSheet(GetComponent).PageControl)
  else
    PageControl := TModernPageControl(GetComponent);
  case Index of
    0:
    begin
      TabSheet := TModernTabSheet(Designer.CreateComponent(TModernTabSheet,
        PageControl, 0, 0, 0, 0));
      TabSheet.Parent := PageControl;
      TabSheet.PageControl := PageControl;
      Designer.Modified;
    end;
    1:
    begin
      if (PageControl.ActivePageIndex < PageControl.PageCount - 1) then
        PageControl.ActivePageIndex := PageControl.ActivePageIndex + 1;
    end;
    2:
    begin
      if (PageControl.ActivePageIndex > 0) then
        PageControl.ActivePageIndex := PageControl.ActivePageIndex - 1;
    end;
  else
    if PageControl.ActivePageIndex >= 0 then
      PageControl.ActivePage.Free;
    Designer.Modified;
  end;
end;

function TModernPageControlEditor.GetVerb(Index: Integer): string;
begin
  case Index of
    0: Result := 'Ne&w Page';
    1: Result := 'Ne&xt Page';
    2: Result := '&Prevous Page';
  else
    Result := '&Delete Page';
  end;
end;

function TModernPageControlEditor.GetVerbCount: Integer;
begin
  Result := 4;
end;

end.
