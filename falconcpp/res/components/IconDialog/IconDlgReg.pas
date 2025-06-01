unit IconDlgReg;

interface

uses
  Forms, Classes, DesignEditors, DesignIntf;

type
  TIconDialogProperty = class(TComponentEditor)
  public
    procedure ExecuteVerb(Index: Integer); override;
    function GetVerb(Index: Integer): string; override;
    function GetVerbCount: Integer; override;
  end;

procedure Register;

implementation

uses IconDialog;

{ TIconDialogProperty }

procedure TIconDialogProperty.ExecuteVerb(Index: Integer);
var
  Dialog: TIconDialog;
begin
  case Index of
    0: begin
         Dialog := TIconDialog.Create(Application);
         Dialog.IconIndex := TIconDialog(Component).IconIndex;
         Dialog.FileName := TIconDialog(Component).FileName;
         if Dialog.Execute then
         begin
           TIconDialog(Component).IconIndex := Dialog.IconIndex;
           TIconDialog(Component).FileName := Dialog.FileName;
         end;
         Dialog.Free;
       end;
  end;
end;

function TIconDialogProperty.GetVerb(Index: Integer): string;
begin
  case Index of
    0: Result := 'Test Dialog';
  end;
end;

function TIconDialogProperty.GetVerbCount: Integer;
begin
  Result := 1;
end;

procedure Register;
begin
  RegisterComponents('Dialogs', [TIconDialog]);
  RegisterComponentEditor(TIconDialog, TIconDialogProperty);
end;

end.
 