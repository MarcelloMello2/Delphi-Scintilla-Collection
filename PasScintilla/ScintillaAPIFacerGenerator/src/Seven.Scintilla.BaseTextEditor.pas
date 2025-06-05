unit Seven.Scintilla.BaseTextEditor;

{$IFDEF FPC}
  {$MODE DELPHI}
{$ENDIF}

interface

{$SCOPEDENUMS ON}

uses
  Seven.Scintilla.Types,

  Winapi.Windows,
  Winapi.Messages,
  System.Classes,
  System.SysUtils,
  Vcl.Controls;

const
  SCINTILLA_DLL = 'Scintilla.dll';

type

  TScintillaWnd = record
    WindowHandle: HWND;
    Visible: Boolean;
  end;

{ TBaseSciTextEditor }

  TSciAccessMethod = (Windows, Direct);

  TSciDirectFunction = function(APointer: Pointer; AMessage: Integer; WParam: WPARAM; LParam: LPARAM): LRESULT; cdecl;

  TSciDirectStatusFunction = function(APointer: Pointer; AMessage: Integer; WParam: WPARAM; LParam: LPARAM; var AStatus: Integer): LRESULT; cdecl;

  TBaseSciTextEditor = class(TWinControl)
  strict private
    FSciDllHandle: HMODULE;
    FSciDllModule: String;

    FDirectPointer: Pointer;
    FDirectFunction: TSciDirectFunction;
    FDirectStatusFunction: TSciDirectStatusFunction;
    FAccessMethod: TSciAccessMethod;
    FNativeThreadId: DWORD;

    FStoredWnd: TScintillaWnd;

    procedure SetSciDllModule(const Value: String);

    procedure LoadSciLibraryIfNeeded;
    procedure FreeSciLibrary;

    procedure DoStoreWnd;
    procedure DoRestoreWnd(const Params: TCreateParams);

  strict protected
    procedure CreateWnd; override;
    procedure CreateParams(var Params: TCreateParams); override;

    function IsRecreatingWnd: Boolean;
    procedure CreateWindowHandle(const Params: TCreateParams); override;
    procedure DestroyWindowHandle; override;

    procedure WMCreate(var AMessage: TWMCreate); message WM_CREATE;
    procedure WMDestroy(var AMessage: TWMDestroy); message WM_DESTROY;

    procedure WMEraseBkgnd(var AMessage: TWmEraseBkgnd); message WM_ERASEBKGND;
    procedure WMGetDlgCode(var AMessage: TWMGetDlgCode); message WM_GETDLGCODE;

    /// <summary>
    ///   Envia mensagem para o controle Scintilla.
    ///   Para a lista de comandos, consulte Seven.Scintilla.SciTypes.pas e a documentação em:
    ///   http://www.scintilla.org/ScintillaDoc.html
    // </summary>
    function SendScintillaEditorMessage(AMessage: UINT; WParam: WPARAM = 0; LParam: LPARAM = 0): LRESULT; virtual;

    /// <summary>
    ///   Nome da DLL Scintilla que será usada.
    ///   Alterar o DllModule recria o controle!
    /// </summary>
    property DllModule: String read FSciDllModule write SetSciDllModule;

    /// <summary>
    /// Método de acesso ao controle Scintilla. Nota da documentação:
    /// No Windows, o esquema de troca de mensagens usado para comunicação
    /// entre o contêiner e o Scintilla é mediado pelo sistema operacional
    /// Função SendMessage e pode levar a um desempenho ruim
    /// ao realizar chamadas intensivas.
    ///
    /// Por padrão, o aqui é utilizar o  modo TSciAccessMethod.Direct
    /// </summary>
    property AccessMethod: TSciAccessMethod read FAccessMethod write FAccessMethod default TSciAccessMethod.Direct;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    procedure DefaultHandler(var AMessage); override;
    procedure MouseWheelHandler(var AMessage: TMessage); override;
  end;

implementation

{ TBaseSciTextEditor }

constructor TBaseSciTextEditor.Create(AOwner: TComponent);
begin
  FSciDllModule := SCINTILLA_DLL;
  FAccessMethod := TSciAccessMethod.Direct;

  inherited Create(AOwner);

  // O Editor não pode usar csAcceptsControls, pois a pintura é realizada por
  // scintilla, portanto, não suporta controles non-handle (como TLabel)
  ControlStyle := ControlStyle + [csOpaque,
                                  csClickEvents,
                                  csDoubleClicks,
                                  csCaptureMouse,
                                  csReflector] - [csSetCaption, csAcceptsControls];

  Width := 320;
  Height := 240;
end;

destructor TBaseSciTextEditor.Destroy;
begin
  if IsRecreatingWnd then
  begin
    WindowHandle := FStoredWnd.WindowHandle;
    FStoredWnd.WindowHandle := 0;
  end;

  inherited Destroy;

  FreeSciLibrary;
end;

procedure TBaseSciTextEditor.SetSciDllModule(const Value: String);
begin
  if Value = FSciDllModule then
    Exit;

  FSciDllModule := Value;

  RecreateWnd();
end;

procedure TBaseSciTextEditor.LoadSciLibraryIfNeeded;
begin
  if FSciDllHandle <> 0 then
    Exit;

  FSciDllHandle := LoadLibrary(PChar(FSciDllModule));
  if FSciDllHandle = 0 then
    RaiseLastOSError;
end;

procedure TBaseSciTextEditor.FreeSciLibrary;
begin
  if FSciDllHandle <> 0 then
  try
    FreeLibrary(FSciDllHandle);
  finally
    FSciDllHandle := 0;
  end;
end;

procedure TBaseSciTextEditor.DoStoreWnd;
begin
  FStoredWnd.Visible := Visible;
  FStoredWnd.WindowHandle := WindowHandle;

  // Simular mensagens passadas pelo DestroyWindow
  SetWindowPos(WindowHandle,
               0, 0, 0, 0, 0,
               SWP_HIDEWINDOW or
               SWP_NOACTIVATE or
               SWP_NOSIZE or
               SWP_NOMOVE or
               SWP_NOZORDER);
  Winapi.Windows.SetParent(FStoredWnd.WindowHandle, 0);

  // Self.WindowHandle deve ser definido devido a UpdateBounds chamados de WMWindowPosChanged
  // Não podemos definir csDestroyingHandle para evitar esse problema,
  // pois é um campo privado de TWinControl.
  // Chamadas SetParent e SetWindowPos de WMWindowPosChanged
  WindowHandle := 0;

  // TODO: WNDProc?
end;

procedure TBaseSciTextEditor.DoRestoreWnd(const Params: TCreateParams);
var
  Flags: UINT;
begin
  WindowHandle := FStoredWnd.WindowHandle;
  FStoredWnd.WindowHandle:= 0;

  // TODO: WNDProc?

  Winapi.Windows.SetParent(WindowHandle, Params.WndParent);

  Flags := SWP_FRAMECHANGED or
           SWP_NOACTIVATE or
           SWP_NOCOPYBITS or
           SWP_NOOWNERZORDER or
           SWP_NOZORDER;

  if FStoredWnd.Visible then
    Flags := Flags or SWP_SHOWWINDOW;

  // Restaurar tamanho e posição anteriores (semelhante ao feito pelo CreateWindowEx)
  SetWindowPos(WindowHandle, 0, Params.X, Params.Y, Params.Width, Params.Height, Flags);
end;

procedure TBaseSciTextEditor.CreateWnd();
begin
  // Carregue o Scintilla se ele ainda não estiver carregado.
  // A biblioteca deve ser carregada antes da subclassificação/criação da janela
  LoadSciLibraryIfNeeded();

  inherited CreateWnd();
end;

procedure TBaseSciTextEditor.CreateParams(var Params: TCreateParams);
begin
  inherited CreateParams(Params);

  // Subclasse Scintilla - E a classe foi registrada no processo de carregamento da DLL
  CreateSubClass(Params, 'SCINTILLA');
end;

function TBaseSciTextEditor.IsRecreatingWnd(): Boolean;
begin
  Result := FStoredWnd.WindowHandle <> 0;
end;

procedure TBaseSciTextEditor.CreateWindowHandle(const Params: TCreateParams);
begin
  if IsRecreatingWnd then
    DoRestoreWnd(Params)
  else
    inherited CreateWindowHandle(Params);
end;

procedure TBaseSciTextEditor.DestroyWindowHandle;
begin
  if (csDestroying in ComponentState) or (csDesigning in ComponentState) then
    inherited DestroyWindowHandle
  else
    DoStoreWnd;
end;

procedure TBaseSciTextEditor.WMCreate(var AMessage: TWMCreate);
begin
  inherited;

  FDirectFunction := TSciDirectFunction(Winapi.Windows.SendMessage(
    WindowHandle, SCI_GETDIRECTFUNCTION, 0, 0));
  FDirectStatusFunction := TSciDirectStatusFunction(Winapi.Windows.SendMessage(
    WindowHandle, SCI_GETDIRECTSTATUSFUNCTION, 0, 0));
  FDirectPointer := Pointer(Winapi.Windows.SendMessage(
    WindowHandle, SCI_GETDIRECTPOINTER, 0, 0));

  // Salvar o ID da Thread atual
  FNativeThreadId := Winapi.Windows.GetCurrentThreadId();
end;

procedure TBaseSciTextEditor.WMDestroy(var AMessage: TWMDestroy);
begin
  inherited;

  // Não é mais válido após a destruição da janela
  FDirectFunction := nil;
  FDirectStatusFunction := nil;
  FDirectPointer := nil;
end;

procedure TBaseSciTextEditor.WMEraseBkgnd(var AMessage: TWmEraseBkgnd);
begin
  // WMEraseBkgnd necessário quando DoubleBuffered=True (Issue 23)
  if (csDesigning in ComponentState) or DoubleBuffered then
    inherited
  else
    // Apagar background não realizado, evitar cintilação
    AMessage.Result := 1;
end;

procedure TBaseSciTextEditor.WMGetDlgCode(var AMessage: TWMGetDlgCode);
begin
  inherited;

  // Permitir que códigos de tecla como Enter, Tab, Setas e outros sejam passados ​​para o Scintilla
  AMessage.Result := AMessage.Result or DLGC_WANTARROWS or DLGC_WANTCHARS;
  AMessage.Result := AMessage.Result or DLGC_WANTTAB;
  AMessage.Result := AMessage.Result or DLGC_WANTALLKEYS;
end;

procedure TBaseSciTextEditor.DefaultHandler(var AMessage);
begin
  // No modo de design, há um AV ao clicar no controle sem esta solução alternativa
  // É um problema de wParam HDC vs. PAINTSTRUCT:
  (*
  LRESULT ScintillaWin::WndPaint(uptr_t wParam) {
    ...
    PAINTSTRUCT ps;
    PAINTSTRUCT *pps;

    bool IsOcxCtrl = (wParam != 0); // if wParam != 0, it contains
                     // a PAINSTRUCT* from the OCX
  *)

  if (TMessage(AMessage).Msg = WM_PAINT) and (TWMPaint(AMessage).DC <> 0) then
  begin
    // Problema 23: Problemas de pintura quando DoubleBuffered é True
    //
    // A VCL envia WM_PAINT com wParam=DC quando deseja pintar em um DC específico (como ao usar DoubleBuffered).
    // No entanto, o Scintilla ameaça wParam como um PAINTSTRUCT (problema relacionado ao OCX).
    // Anteriormente, havia uma solução alternativa para definir wParam:=0, pois isso causava AVs no IDE.
    // Agora, em vez dessa solução alternativa, simula a pintura por WM_PRINTCLIENT, como esperado em wParam=DC.
    // O Scintilla manipula essa mensagem - veja: http://sourceforge.net/p/scintilla/feature-requests/173/
    TMessage(AMessage).Msg := WM_PRINTCLIENT;
    // TWMPrintClient(AMessage).DC são iguais a um TWMPaint(AMessage).DC

    // Os sinalizadores WM_PRINTCLIENT não são usados agora, mas passam pelo menos PRF_CLIENT para
    // possíveis mudanças futuras no Scintilla
    TWMPrintClient(AMessage).Flags := PRF_CLIENT;
  end;

  inherited;
end;

procedure TBaseSciTextEditor.MouseWheelHandler(var AMessage: TMessage);

  function VCLBugWorkaround_ShiftStateToKeys(AShiftState: TShiftState): Word;
  begin
    // Reverse function for Forms.KeysToShiftState
    // However it doesn't revert MK_XBUTTON1/MK_XBUTTON2
    // but Scintilla as of version 3.25 doesn't use it.
    Result := 0;

    if ssShift in AShiftState then
      Result := Result or MK_SHIFT;
    if ssCtrl in AShiftState then
      Result := Result or MK_CONTROL;
    if ssLeft in AShiftState then
      Result := Result or MK_LBUTTON;
    if ssRight in AShiftState then
      Result := Result or MK_RBUTTON;
    if ssMiddle in AShiftState then
      Result := Result or MK_MBUTTON;
  end;

begin
  inherited MouseWheelHandler(AMessage);

  // Se a mensagem não foi manipulada pelos eventos OnMouseWheel* ...
  if AMessage.Result = 0 then
  begin
    // TControl.WMMouseWheel altera os parâmetros WM_MOUSEWHEEL,
    // mas não os reverte ao passar a mensagem.
    //
    // Como solução alternativa, tente reverter o dano causado por TControl.WMMouseWheel:
    // TCMMouseWheel(Message).ShiftState := KeysToShiftState(Message.Keys);
    // A mensagem pode não estar completa devido à ausência dos sinalizadores MK_XBUTTON1/MK_XBUTTON2
    // , porém o Scintilla não os utiliza atualmente.
    TWMMouseWheel(AMessage).Keys := VCLBugWorkaround_ShiftStateToKeys(TCMMouseWheel(AMessage).ShiftState);

    // Passe para TWinControl.DefaultHandler->CallWindowProc(WM_MOUSEWHEEL)->Scintilla
    // e marque-o como manipulado para que TControl.WMMouseWheel não chame o método herdado (que chama o DefaultHandler)
    inherited DefaultHandler(AMessage);
    AMessage.Result := 1;
  end;
end;

function TBaseSciTextEditor.SendScintillaEditorMessage(AMessage: UINT; WParam: WPARAM; LParam: LPARAM): LRESULT;
begin
  HandleNeeded();

  { Ver...http://www.scintilla.org/ScintillaDoc.html#DirectAccess

    De acordo com a documentação, a função direta é usada para velocidade...

    "No Windows, o esquema de troca de mensagens usado para comunicação entre o contêiner
    e o Scintilla é mediado pela função SendMessage do sistema operacional e pode
    levar a um desempenho ruim ao fazer chamadas intensivas. Para evitar essa sobrecarga, o Scintilla
    fornece mensagens que permitem chamar a função de mensagem do Scintilla diretamente."

    Também de acordo com a documentação, SendMessage deve ser usado quando chamado de uma thread diferente...

    "Embora seja mais rápido, essa chamada direta causará problemas se realizada de uma thread diferente
    da thread nativa da janela do Scintilla, nesse caso
    SendMessage(hSciWnd, SCI_*, wParam, lParam) deve ser usado para sincronizar com a
    thread da janela."

    Use SendMessage() quando necessário. Esta é uma pequena adaptação do código original,
    que é comentado abaixo.
  }

  if (FAccessMethod = TSciAccessMethod.Windows) or
    (not Assigned(FDirectFunction)) or
    (not Assigned(FDirectPointer)) or
    (Winapi.Windows.GetCurrentThreadId() <> FNativeThreadId) then
  begin
    Result := Winapi.Windows.SendMessage(Self.Handle, AMessage, WParam, LParam);
  end
  else
  begin
    Result := FDirectFunction(FDirectPointer, AMessage, WParam, LParam);
  end;
end;

end.

