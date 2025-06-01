unit SplashScreenReg;

interface

uses
  Windows, Classes, DesignEditors, DesignIntf;

type
  TSplashScreenProperty = class(TComponentEditor)
  public
    procedure ExecuteVerb(Index: Integer); override;
    function GetVerb(Index: Integer): string; override;
    function GetVerbCount: Integer; override;
  end;

procedure Register;

implementation

uses SplashScreen;

{ TSplashScreenProperty }

procedure TSplashScreenProperty.ExecuteVerb(Index: Integer);
var
  SS: TSplashScreen;
begin
  case Index of
    0: begin
         if Assigned(TSplashScreen(Component).Picture.Graphic) then
         begin
           SS := TSplashScreen.Create(nil);
           SS.WaitTime := TSplashScreen(Component).WaitTime;
           SS.TimeOut := TSplashScreen(Component).TimeOut;
           SS.TimeIn := TSplashScreen(Component).TimeIn;
           SS.TransparentColor := TSplashScreen(Component).TransparentColor;
           SS.Picture := TSplashScreen(Component).Picture;
           SS.Show;
           SS.Hide;
           SS.Free;
         end
         else
           MessageBox(0, PChar('Can' + #39 + 't load resource in design state!'),
             'TSplashScreen', MB_ICONEXCLAMATION);
       end;
  end;
end;

function TSplashScreenProperty.GetVerb(Index: Integer): string;
begin
  case Index of
    0: Result := 'Test SplashScreen';
  end;
end;

function TSplashScreenProperty.GetVerbCount: Integer;
begin
  Result := 1;
end;

procedure Register;
begin
  RegisterComponents('Additional', [TSplashScreen]);
  RegisterComponentEditor(TSplashScreen, TSplashScreenProperty);
end;

end.
 