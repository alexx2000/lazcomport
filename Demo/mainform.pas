unit MainForm;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, StdCtrls,
  Buttons, ExtCtrls, Menus, EditBtn, CPortCtl, CPort, IniFiles;

type

  { TfrmMain }

  TfrmMain = class(TForm)
    btnOpenClose: TBitBtn;
    btnSend1: TButton;
    btnSend10: TButton;
    btnSend2: TButton;
    btnSend3: TButton;
    btnSend4: TButton;
    btnSend5: TButton;
    btnSend6: TButton;
    btnSend7: TButton;
    btnSend8: TButton;
    btnSend9: TButton;
    chkLog: TCheckBox;
    chkDTR: TCheckBox;
    chkRTS: TCheckBox;
    Combo1: TComComboBox;
    Combo2: TComComboBox;
    Combo3: TComComboBox;
    Combo4: TComComboBox;
    Combo5: TComComboBox;
    Combo6: TComComboBox;
    ComPort: TComPort;
    ComTerminal1: TComTerminal;
    Copy1: TMenuItem;
    edtSend1: TEdit;
    edtSend10: TEdit;
    edtSend2: TEdit;
    edtSend3: TEdit;
    edtSend4: TEdit;
    edtSend5: TEdit;
    edtSend6: TEdit;
    edtSend7: TEdit;
    edtSend8: TEdit;
    edtSend9: TEdit;
    fneLog: TFileNameEdit;
    grbLines: TGroupBox;
    grbSettings: TGroupBox;
    grbLog: TGroupBox;
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    Label4: TLabel;
    Label5: TLabel;
    Label6: TLabel;
    Panel2: TPanel;
    Panel3: TPanel;
    Paste1: TMenuItem;
    PopupMenu1: TPopupMenu;
    procedure btnOpenCloseClick(Sender: TObject);
    procedure btnSendClick(Sender: TObject);
    procedure chkDTRChange(Sender: TObject);
    procedure chkLogChange(Sender: TObject);
    procedure chkRTSChange(Sender: TObject);
    procedure ComTerminal1StrRecieved(Sender: TObject; var Str: string);
    procedure Copy1Click(Sender: TObject);
    procedure FormClose(Sender: TObject; var CloseAction: TCloseAction);
    procedure FormCreate(Sender: TObject);
    procedure Paste1Click(Sender: TObject);
  private
    FLog: TextFile;
    FSecond: Boolean;
    FIni: TMemIniFile;
    FSend: array of TEdit;
    procedure ApplySettings;
    procedure SetButtonGlyph(AButton: TBitBtn; AKind: TBitBtnKind);
  public
    { public declarations }
  end;

var
  frmMain: TfrmMain;

implementation

{$R *.lfm}

uses
  Clipbrd;

{ TfrmMain }

procedure TfrmMain.btnOpenCloseClick(Sender: TObject);
begin
  if btnOpenClose.Kind = bkAll then
  begin
    ApplySettings;
    ComPort.Open;
    btnOpenClose.Kind:= bkCustom;
    btnOpenClose.Caption:= 'Close';
    btnOpenClose.ModalResult:= mrNone;
    SetButtonGlyph(btnOpenClose, bkClose);
  end
  else begin
    ComPort.Close;
    btnOpenClose.Kind:= bkAll;
    btnOpenClose.Caption:= 'Open';
    btnOpenClose.ModalResult:= mrNone;
    SetButtonGlyph(btnOpenClose, bkOK);
  end;
  grbLines.Enabled:= (btnOpenClose.Kind <> bkAll);
  grbSettings.Enabled:= (btnOpenClose.Kind = bkAll);
end;

procedure TfrmMain.btnSendClick(Sender: TObject);
begin
  ComPort.WriteStr(FSend[TButton(Sender).Tag].Text + LineEnding);
end;

procedure TfrmMain.chkDTRChange(Sender: TObject);
begin
  ComPort.SetDTR(chkDTR.Checked);
end;

procedure TfrmMain.chkLogChange(Sender: TObject);
begin
  if not chkLog.Checked then
  begin
    ComTerminal1.OnStrRecieved:= nil;
    if TextRec(FLog).Handle <> UnusedHandle then CloseFile(FLog);
  end
  else try
    AssignFile(FLog, fneLog.FileName);
    if FileExists(fneLog.FileName) and (FSecond = False) then
    begin
      Append(FLog)
    end
    else begin
      Rewrite(FLog);
    end;
    ComTerminal1.OnStrRecieved:= @ComTerminal1StrRecieved;
  except
    on E: Exception do MessageDlg(E.Message, mtError, [mbOK], 0, mbOK);
  end;
  FSecond := True;
end;

procedure TfrmMain.chkRTSChange(Sender: TObject);
begin
  ComPort.SetRTS(chkRTS.Checked);
end;

procedure TfrmMain.ComTerminal1StrRecieved(Sender: TObject; var Str: string);
begin
  Write(FLog, Str);
end;

procedure TfrmMain.Copy1Click(Sender: TObject);
begin

end;

procedure TfrmMain.FormClose(Sender: TObject; var CloseAction: TCloseAction);
var
  Index: Integer;
begin
  if Assigned(FIni) then begin
    FIni.WriteString('Serial', 'Port', ComPort.Port);
    FIni.WriteString('Serial', 'BaudRate', BaudRateToStr(ComPort.BaudRate));
    FIni.WriteString('Serial', 'FlowControl', FlowControlToStr(ComPort.FlowControl.FlowControl));
    FIni.WriteBool('Log', 'Enabled', chkLog.Checked);
    FIni.WriteString('Log', 'FileName', fneLog.FileName);
    for Index:= Low(FSend) to High(FSend) do
    begin
      FIni.WriteString('Command', 'Send' + IntToStr(Index), FSend[Index].Text);
    end;
    FIni.UpdateFile;
    FIni.Free;
  end;
end;

procedure TfrmMain.FormCreate(Sender: TObject);
var
  I, J: Integer;
  FileName: String;
begin
  J:= 0;
  SetLength(FSend, 10);
  ConfigExtension:= '.ini';
  TextRec(FLog).Handle:= UnusedHandle;
  FileName:= ExtractFilePath(Application.ExeName) + ApplicationName + ConfigExtension;
  if not FileExists(FileName) then FileName:= GetAppConfigFile(False);

  FIni := TMemIniFile.Create(FileName);
  ComPort.Port := FIni.ReadString('Serial', 'Port', ComPort.Port);
  ComPort.BaudRate := StrToBaudRate(FIni.ReadString('Serial', 'BaudRate', '115200'));
  ComPort.FlowControl.FlowControl := StrToFlowControl(FIni.ReadString('Serial', 'FlowControl', 'None'));

  fneLog.FileName:= FIni.ReadString('Log', 'FileName', EmptyStr);
  chkLog.Checked:= FIni.ReadBool('Log', 'Enabled', False);

  for I:= 0 to grbSettings.ControlCount - 1 do
  begin
    if grbSettings.Controls[I] is TComComboBox then
    begin
      TComComboBox(grbSettings.Controls[I]).UpdateSettings;
    end;
  end;

  for I:= 0 to Panel3.ControlCount - 1 do
  begin
    if Panel3.Controls[I] is TEdit then
    begin
      FSend[J]:= TEdit(Panel3.Controls[I]);
      FSend[J].Text:= FIni.ReadString('Command', 'Send' + IntToStr(J), EmptyStr);
      Inc(J);
    end;
  end;
end;

procedure TfrmMain.Paste1Click(Sender: TObject);
begin
  ComPort.WriteStr(Clipboard.AsText);
end;

procedure TfrmMain.ApplySettings;
begin
  ComPort.BeginUpdate;
  Combo1.ApplySettings;
  Combo2.ApplySettings;
  Combo3.ApplySettings;
  Combo4.ApplySettings;
  Combo5.ApplySettings;
  Combo6.ApplySettings;
  ComPort.EndUpdate;
end;

procedure TfrmMain.SetButtonGlyph(AButton: TBitBtn; AKind: TBitBtnKind);
var
  CustomGlyph: TGraphic;
begin
  CustomGlyph := GetLCLDefaultBtnGlyph(AKind);
  if CustomGlyph <> nil then
  begin
    AButton.Glyph.Assign(CustomGlyph);
    CustomGlyph.Free;
  end;
end;

end.

