unit MainForm;

{$mode objfpc}{$H+}

interface

uses
  VideoCapture, videodev2, YUV2RGB,
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics,
  Dialogs, StdCtrls, ExtCtrls, ComCtrls, Grids;

type

  { TFormMain }

  TFormMain = class(TForm)
    ButtonDefaultControls: TButton;
    ButtonUpdateControls: TButton;
    CheckBoxOpenClose: TCheckBox;
    CheckBoxCapture: TCheckBox;
    EditBufferCount: TEdit;
    EditFrameRate: TEdit;
    EditDevice: TEdit;
    Label1: TLabel;
    FrameRate: TLabel;
    LabelError: TLabel;
    MemoInfo: TMemo;
    PaintBox: TPaintBox;
    PanelControls: TScrollBox;
    StatusBar: TStatusBar;
    Video: TVideo4L2Device;
    procedure ButtonDefaultControlsClick(Sender: TObject);
    procedure ButtonUpdateControlsClick(Sender: TObject);
    procedure CheckBoxOpenCloseChange(Sender: TObject);
    procedure CheckBoxCaptureChange(Sender: TObject);
    procedure EditBufferCountEditingDone(Sender: TObject);
    procedure EditFrameRateEditingDone(Sender: TObject);
    procedure EditDeviceEditingDone(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure LabelErrorClick(Sender: TObject);
    procedure VideoFrameSynchronized(Sender: TObject; Buffer: pointer;
      Size: integer; Error: boolean);
  private
    { private declarations }
    BMP: TBitmap;
    prevTicks: QWord;
    UpdatingControls: boolean;
    procedure ControlTrackBarChange(Sender: TObject);
    procedure ControlComboBoxChange(Sender: TObject);
    procedure ControlButtonClick(Sender: TObject);
    procedure ControlCheckBoxChange(Sender: TObject);
    procedure ClearPanelControls;
    procedure ShowPanelControls;
    procedure UpdatePanelControls;
    procedure UpdateParams;
  public
    { public declarations }
  end;

var
  FormMain: TFormMain;

implementation

{$R *.lfm}

{ TFormMain }

procedure TFormMain.FormCreate(Sender: TObject);
begin
  BMP:=TBitmap.Create;
  BMP.Width:=PaintBox.Width;
  BMP.Height:=PaintBox.Height;
  BMP.PixelFormat:=pf24bit; // Byte order: BGR. pf32bit gives inverted RGB order for some reason
  UpdateParams;
end;

procedure TFormMain.FormDestroy(Sender: TObject);
begin
  Video.Free; // free Video BEFORE drawable bitmap or Synchronize will raise AV
  BMP.Free;
end;

procedure TFormMain.CheckBoxOpenCloseChange(Sender: TObject);
begin
  if UpdatingControls then exit;
  Video.Open:=CheckBoxOpenClose.Checked;
  if Video.Open then begin
    Video.SetControlValue(V4L2_CID_POWER_LINE_FREQUENCY,V4L2_CID_POWER_LINE_FREQUENCY_50HZ);
    Video.SetControlValue(V4L2_CID_EXPOSURE_AUTO_PRIORITY,0);
    ShowPanelControls;
    UpdatePanelControls;
  end else begin
    ClearPanelControls;
  end;
  UpdateParams;
end;

procedure TFormMain.CheckBoxCaptureChange(Sender: TObject);
var flag:boolean;
begin
  if UpdatingControls then exit;
  flag:=CheckBoxCapture.Checked and not Video.Open;
  Video.Capture:=CheckBoxCapture.Checked;
  if flag then begin
    ShowPanelControls;
    UpdatePanelControls;
  end;
  UpdateParams;
end;

procedure TFormMain.EditBufferCountEditingDone(Sender: TObject);
begin
  Video.BufferCount:=StrToInt(EditBufferCount.Text);
end;

procedure TFormMain.EditFrameRateEditingDone(Sender: TObject);
begin
  Video.FrameRate:=StrToInt(EditFrameRate.Text);
  UpdateParams;
end;

procedure TFormMain.EditDeviceEditingDone(Sender: TObject);
begin
  Video.Device:=EditDevice.Text;
  UpdateParams;
end;

procedure TFormMain.UpdateParams;
begin
  UpdatingControls:=True;
  CheckBoxOpenClose.Checked:=Video.Open;
  CheckBoxCapture.Checked:=Video.Capture;
  EditBufferCount.Text:=IntToStr(Video.BufferCount);
  EditFrameRate.Text:=IntToStr(Video.FrameRate);
  UpdatingControls:=False;
end;

function ControlTypeToStr(t:longword):string;
begin
  case t of
    V4L2_CTRL_TYPE_INTEGER:      Result:='int';
    V4L2_CTRL_TYPE_BOOLEAN:      Result:='bool';
    V4L2_CTRL_TYPE_MENU:         Result:='menu';
    V4L2_CTRL_TYPE_INTEGER_MENU: Result:='int_menu';
    V4L2_CTRL_TYPE_BITMASK:      Result:='bitmask';
    V4L2_CTRL_TYPE_BUTTON:       Result:='button';
    V4L2_CTRL_TYPE_INTEGER64:    Result:='int64';
    V4L2_CTRL_TYPE_STRING:       Result:='string';
  else
    Result:='???';
  end;
end;

procedure TFormMain.ClearPanelControls;
var i: integer;
begin
  for i:=PanelControls.ControlCount-1 downto 0 do begin
    PanelControls.Controls[i].Free;
  end;
end;

procedure TFormMain.ShowPanelControls;
var i,j:integer;
  ctop: integer;
  LabelControl: TLabel;
  NewControl: TControl;
  TrackBar: TTrackBar;
  CheckBox: TCheckBox;
  ComboBox: TComboBox;
  Button: TButton;
begin
  MemoInfo.Clear;
  for i:=0 to Video.ControlsInfo.Count-1 do begin
    with Video.ControlsInfo[i] do begin
      MemoInfo.Lines.Add(Name+' ('+ControlTypeToStr(ControlType)+') ['+
        IntToStr(Minimum)+'..'+IntToStr(Maximum)+'] ('+IntToStr(DefaultValue)+')');
      if ControlType in [V4L2_CTRL_TYPE_MENU, V4L2_CTRL_TYPE_INTEGER_MENU] then begin
        for j:=0 to Menu.Count-1 do begin
          with Menu[j] do begin
            if TypeInteger then begin
              MemoInfo.Lines.Add('  '+IntToStr(Index)+': (int64) '+Name+' '+IntToHex(Value,8));
            end else begin
              MemoInfo.Lines.Add('  '+IntToStr(Index)+': '+Name)
            end;
          end;
        end;
      end;
    end;
  end;

  UpdatingControls:=True;
  ClearPanelControls;
  ctop:=0;
  for i:=0 to Video.ControlsInfo.Count-1 do begin
    with Video.ControlsInfo[i] do begin
      LabelControl:=TLabel.Create(Self);
      LabelControl.Font.Size:=8;
      LabelControl.Caption:=Name;
      LabelControl.Top:=14+ctop;
      LabelControl.Left:=4;
      LabelControl.Width:=100;
      LabelControl.Parent:=PanelControls;
      case ControlType of
        V4L2_CTRL_TYPE_INTEGER: begin
          TrackBar:=TTrackBar.Create(Self);
          TrackBar.Min:=Minimum;
          TrackBar.Max:=Maximum;
          TrackBar.LineSize:=Step;
          TrackBar.PageSize:=Step;
          TrackBar.Position:=DefaultValue;
          TrackBar.Height:=38;
          TrackBar.OnChange:=@ControlTrackBarChange;
          NewControl:=TrackBar;
        end;
        V4L2_CTRL_TYPE_BOOLEAN: begin
          CheckBox:=TCheckBox.Create(Self);
          CheckBox.Checked:=Boolean(DefaultValue);
          CheckBox.OnChange:=@ControlCheckBoxChange;
          NewControl:=CheckBox;
        end;
        V4L2_CTRL_TYPE_MENU: begin
          ComboBox:=TComboBox.Create(Self);
          ComboBox.Style:=csDropDownList;
          for j:=0 to Menu.Count-1 do begin
            ComboBox.Items.Add(Menu[j].Name);
          end;
          ComboBox.ItemIndex:=Value; // DefaultValue is menu index, we need item index here, so using Value
          ComboBox.OnChange:=@ControlComboBoxChange;
          NewControl:=ComboBox;
        end;
        V4L2_CTRL_TYPE_INTEGER_MENU: begin
          ComboBox:=TComboBox.Create(Self);
          ComboBox.Style:=csDropDownList;
          for j:=0 to Menu.Count-1 do begin
            ComboBox.Items.Add(Menu[j].Name);
          end;
          ComboBox.ItemIndex:=Value; // DefaultValue is menu index, we need item index here, so using Value
          ComboBox.OnChange:=@ControlComboBoxChange;
          NewControl:=ComboBox;
        end;
        V4L2_CTRL_TYPE_BUTTON: begin
          Button:=TButton.Create(Self);
          Button.OnClick:=@ControlButtonClick;
          NewControl:=Button;
        end;
      else
        // V4L2_CTRL_TYPE_BITMASK:
        // V4L2_CTRL_TYPE_INTEGER64:
        // V4L2_CTRL_TYPE_STRING:
        LabelControl:=TLabel.Create(Self);
        LabelControl.Caption:='('+ControlTypeToStr(ControlType)+')';
        NewControl:=LabelControl;
      end;
      NewControl.Tag:=i;
      NewControl.Font.Size:=8;
      NewControl.Top:=4+ctop;
      NewControl.Left:=4+100;
      NewControl.Width:=170;
      NewControl.Parent:=PanelControls;
      ctop:=ctop+40;
    end;
  end;
  UpdatingControls:=False;
end;

procedure TFormMain.UpdatePanelControls;
var
  i:integer;
  Control:TControl;
begin
  UpdatingControls:=True;
  Video.GetControlValues;
  for i:=0 to Video.ControlsInfo.Count-1 do begin
    with Video.ControlsInfo[i] do begin
      Control:=PanelControls.Controls[i*2+1];
      case ControlType of
        V4L2_CTRL_TYPE_INTEGER: begin
          (Control as TTrackBar).Position:=Value;
        end;
        V4L2_CTRL_TYPE_BOOLEAN: begin
          (Control as TCheckBox).Checked:=Boolean(Value);
        end;
        V4L2_CTRL_TYPE_MENU: begin
          (Control as TComboBox).ItemIndex:=Value;
        end;
        V4L2_CTRL_TYPE_INTEGER_MENU: begin
          (Control as TComboBox).ItemIndex:=Value;
        end;
        V4L2_CTRL_TYPE_BUTTON: begin
          // no value
        end;
      else
        // V4L2_CTRL_TYPE_BITMASK:
        // V4L2_CTRL_TYPE_INTEGER64:
        // V4L2_CTRL_TYPE_STRING:
      end;
    end;
  end;
  UpdatingControls:=False;
end;

procedure TFormMain.ButtonUpdateControlsClick(Sender: TObject);
begin
  UpdatePanelControls;
end;

procedure TFormMain.ButtonDefaultControlsClick(Sender: TObject);
begin
  Video.SetDefaulControlValues;
  UpdatePanelControls;
end;

procedure TFormMain.ControlTrackBarChange(Sender: TObject);
begin
  if UpdatingControls then exit;
  with Sender as TTrackBar do begin
    Video.ControlsInfo[Tag].SetValue(Position);
  end;
end;

procedure TFormMain.ControlCheckBoxChange(Sender: TObject);
begin
  if UpdatingControls then exit;
  with Sender as TCheckBox do begin
    Video.ControlsInfo[Tag].SetValue(Integer(Checked));
  end;
end;

procedure TFormMain.ControlButtonClick(Sender: TObject);
begin
  if UpdatingControls then exit;
  with Sender as TButton do begin
    Video.ControlsInfo[Tag].SetValue(0); // any value is ok for button control
  end;
end;

procedure TFormMain.ControlComboBoxChange(Sender: TObject);
begin
  if UpdatingControls then exit;
  with Sender as TComboBox do begin
    Video.ControlsInfo[Tag].SetValue(ItemIndex); // item index will be recalculated to menu index
  end;
end;

procedure TFormMain.LabelErrorClick(Sender: TObject);
begin
  LabelError.Caption:='';
end;

procedure TFormMain.VideoFrameSynchronized(Sender: TObject; Buffer: pointer;
  Size: integer; Error: boolean);
var ticks:QWord;
begin
  if Size<>(PaintBox.Width*PaintBox.Height*2{YUYV}) then begin
    LabelError.Caption:='Invalid buffer length '+IntToStr(Size);
  end;
  if Error then begin
    LabelError.Caption:='Frame with recoverable error received';
  end;

  BMP.BeginUpdate;
  YUYV_to_BGRA(PLongWord(Buffer), PLongWord(BMP.RawImage.Data), BMP.Width*BMP.Height);
  BMP.EndUpdate;
  PaintBox.Canvas.Draw(0,0,BMP);

  ticks:=GetTickCount64;
  StatusBar.SimpleText:=IntToStr(round(1000.0/(ticks-prevTicks)))+' FPS';
  StatusBar.Refresh;
  prevTicks:=ticks;
end;

end.

