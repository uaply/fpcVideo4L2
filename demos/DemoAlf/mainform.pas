unit MainForm;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics,
  Dialogs, StdCtrls, ExtCtrls, ComCtrls,
  FPReadJPEG,IntfGraphics,FPImage,RGB32writer,
  VideoCapture, videodev2
  ;

type

  { TFormMain }

  TFormMain = class(TForm)
    Bevel1: TBevel;
    ButtonDefaultControls: TButton;
    ButtonUpdateControls: TButton;
    CheckBoxCapture: TCheckBox;
    CheckBoxOpenClose: TCheckBox;
    EditBufferCount: TEdit;
    EditDevice: TEdit;
    EditFrameRate: TEdit;
    FrameRate: TLabel;
    Label1: TLabel;
    Label2: TLabel;
    BytesLabel: TLabel;
    BytesNumberLabel: TLabel;
    Memo1: TMemo;
    MemoInfo: TMemo;
    PageControl1: TPageControl;
    PaintBox: TPaintBox;
    PanelControls: TScrollBox;
    StatusBar: TStatusBar;
    CameraSettingsTabSheet: TTabSheet;
    GlobalSettingsTabSheet: TTabSheet;

    procedure ButtonDefaultControlsClick(Sender: TObject);
    procedure ButtonUpdateControlsClick(Sender: TObject);
    procedure CheckBoxOpenCloseChange(Sender: TObject);
    procedure CheckBoxCaptureChange(Sender: TObject);
    procedure EditBufferCountEditingDone(Sender: TObject);
    procedure EditFrameRateEditingDone(Sender: TObject);
    procedure EditDeviceEditingDone(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure PaintBoxClick(Sender: TObject);
    procedure VideoFrameSynchronized(Sender: TObject; Buffer: pointer;
      Size: integer; Error: boolean);
  private
    { private declarations }
    Video            : TVideo4L2Device;
    BMP              : TBitmap;
    prevTicks        : QWord;
    UpdatingControls : boolean;
    FScanInProgress  : boolean;
    FSaveInProgress  : boolean;
    FFrameTake       : word;

    // for image capturing
    aMS        : TMemoryStream;
    aImage     : TFPCompactImgRGB16Bit;
    aImgReader : TFPReaderJpeg;
    aImgWriter : TFPWriterRGB32;

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

uses
  YUV2RGB;

{ TFormMain }

procedure TFormMain.FormCreate(Sender: TObject);
var
  ScaleDivisor:word;
begin
  FSaveInProgress:= False;

  Video:=TVideo4L2Device.Create(Self);

  Video.PixelFormat := V4L2_PIX_FMT_MJPEG;

  Video.Width := 1920;
  Video.Height := 1080;
  Video.FrameRate := 2;
  Video.BufferCount := 4;

  Video.OnFrameSynchronized := @VideoFrameSynchronized;

  // BMP for on-form preview
  BMP:=TBitmap.Create;
  BMP.PixelFormat:=pf24bit;
  BMP.SetSize(PaintBox.Width,PaintBox.Height);

  aMS        := TMemoryStream.Create;
  aImgWriter := TFPWriterRGB32.Create;
  aImgReader := TFPReaderJpeg.Create;

  aImgReader.Performance:=jpBestSpeed;
  aImgReader.Smoothing:=False;
  case aImgReader.Scale of
    jsFullSize:ScaleDivisor:=1;
    jsHalf:ScaleDivisor:=2;
    jsQuarter:ScaleDivisor:=4;
    jsEighth:ScaleDivisor:=8;
  else
    ScaleDivisor:=1;
  end;

  aImage     := TFPCompactImgRGB16Bit.Create(Video.Width DIV ScaleDivisor, Video.Height DIV ScaleDivisor);
  aImage.UsePalette:=False;

  UpdateParams;
end;

procedure TFormMain.FormDestroy(Sender: TObject);
begin
  Video.Free; // free Video BEFORE drawable bitmap or Synchronize will raise AV
  if aMS<>nil then aMS.Free;
  if aImage<>nil then aImage.Free;
  if aImgReader<>nil then aImgReader.Free;
  if aImgWriter<>nil then aImgWriter.Free;
  if BMP<>nil then BMP.Free;
end;

procedure TFormMain.PaintBoxClick(Sender: TObject);
begin
  FSaveInProgress := true;
end;

procedure TFormMain.CheckBoxOpenCloseChange(Sender: TObject);
begin
  if UpdatingControls then exit;
  Video.Open:=CheckBoxOpenClose.Checked;
  if Video.Open then
  begin
    //Video.SetControlValue(V4L2_CID_POWER_LINE_FREQUENCY,V4L2_CID_POWER_LINE_FREQUENCY_50HZ);
    //Video.SetControlValue(V4L2_CID_AUTO_WHITE_BALANCE,V4L2_WHITE_BALANCE_AUTO);
    //Video.SetControlValue(V4L2_CID_EXPOSURE_AUTO,V4L2_EXPOSURE_APERTURE_PRIORITY);
    ShowPanelControls;
    UpdatePanelControls;
  end
  else
  begin
    ClearPanelControls;
  end;
  UpdateParams;
end;

procedure TFormMain.CheckBoxCaptureChange(Sender: TObject);
var
  flag:boolean;
begin
  if UpdatingControls then exit;
  flag:=CheckBoxCapture.Checked and not Video.Open;
  Video.Capture:=CheckBoxCapture.Checked;
  if flag then begin
    ShowPanelControls;
    UpdatePanelControls;
  end;
  UpdateParams;
  if Video.Capture
     then MemoInfo.Lines.Add('Capturing started')
     else MemoInfo.Lines.Add('Capturing stopped');

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
      LabelControl.WordWrap:=True;
      LabelControl.AutoSize:=False;
      LabelControl.Font.Size:=8;
      LabelControl.Caption:=Name;
      LabelControl.Top:=14+ctop;
      LabelControl.Left:=4;
      LabelControl.Width:=100;
      LabelControl.Height:=26;
      LabelControl.WordWrap:=True;
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

procedure TFormMain.VideoFrameSynchronized(Sender: TObject; Buffer: pointer;
  Size: integer; Error: boolean);
var
  ticks       : QWord;
  //ReadResult  : TReadResult;
begin

  if (FScanInProgress) then exit;

  { This code will take every 4 frames. }
  inc(FFrameTake);
  //if (FFrameTake mod 4 <> 0) then exit;
  //if (FFrameTake mod 4 <> 0) then exit;

  FScanInProgress:=True;

  try
    BytesNumberLabel.Caption:=IntToStr(Size);

    if Video.PixelFormat = V4L2_PIX_FMT_YUYV then
    begin
      BMP.BeginUpdate;
      //YUYV_to_BGRA(PLongWord(Buffer), PLongWord(BMP.RawImage.Data), BMP.Width*BMP.Height);
      YUYV_to_BGRA16(PLongWord(Buffer), PWord(BMP.RawImage.Data), BMP.Width*BMP.Height);
      BMP.EndUpdate;
      PaintBox.Canvas.Draw(0,0,BMP);
    end;

    if Video.PixelFormat = V4L2_PIX_FMT_MJPEG then
    begin
      aMS.Clear;
      aMS.Position:=0;
      aMS.WriteBuffer(Buffer^,Size);
      aMS.Position:=0;
      aImage.LoadFromStream(aMS,aImgReader);
      aMS.Clear;
      aMS.Position:=0;
      aImage.SaveToStream(aMS,aImgWriter);
      aMS.Position:=0;
      BMP.BeginUpdate;
      Move(aMS.Memory^,PByte(BMP.RawImage.Data)^,Video.Width*Video.Height*aImgWriter.Bpp);
      BMP.EndUpdate;
      PaintBox.Canvas.Draw(0,0,BMP);
    end;

    if FSaveInProgress then begin
      BMP.SaveToFile('/home/pi/pic.bmp');
      FSaveInProgress:= false;
    end;

    ticks:=GetTickCount64;
    StatusBar.SimpleText:=IntToStr(round(1000.0/(ticks-prevTicks)))+' FPS';
    StatusBar.Refresh;
    prevTicks:=ticks;

  finally
    FScanInProgress:=False;
  end;

end;

end.

