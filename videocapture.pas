{
  Video4L2 capture and controls enumeration component for Lazarus
  
  Copyright (C) 2013 Yuriy Pilgun

  This program is provided under the terms of the MIT License.
}
unit VideoCapture;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, videodev2;

{$define NOT_USELIBV4L2}

type
  EVideo4L2Exception = class(Exception)
  end;

  TVideo4L2Device = class;

  TVideoControlMenuItem = class(TCollectionItem)
  private
    FIndex: integer;
    FName: string;
    FInt64Value: int64;
    FTypeInteger: boolean;
  public
    property Index: integer read FIndex write FIndex;
    property Name: string read FName write FName;
    property Int64Value: int64 read FInt64Value write FInt64Value;
    property TypeInteger: boolean read FTypeInteger write FTypeInteger;
  end;

  TVideoControlMenu = class (TCollection)
  private
    function GetItem(Index: integer): TVideoControlMenuItem;
  public
    constructor Create;
    function Add: TVideoControlMenuItem;
    property Item[Index: Integer]: TVideoControlMenuItem read GetItem; default;
  end;

  TVideoControl = class(TCollectionItem)
  private
    FDevice: TVideo4L2Device;
  private
    FId: longword;
    FType: longword;
    FName: string;
    FMinimum: integer;
    FMaximum: integer;
    FStep: integer;
    FDefaultValue: integer;
    FFlagDisabled: boolean;
    FFlagGrabbed: boolean;
    FFlagReadOnly: boolean;
    FFlagUpdate: boolean;
    FFlagInactive: boolean;
    FFlagSlider: boolean;
    FFlagWriteOnly: boolean;
    FFlagVolatile: boolean;
  private
    FMenu: TVideoControlMenu;
    FValue: integer;
    FDefaultValueIndex: integer;
    function FindMenuIndex(val: integer):integer;
  public
    constructor Create(ACollection: TCollection); override;
    destructor Destroy; override;
    function GetValue:integer;
    procedure SetValue(val: integer);
    property Id: longword read FId write FId;
    property ControlType: longword read FType write FType;
    property Name: string read FName write FName;
    property Minimum: integer read FMinimum write FMinimum;
    property Maximum: integer read FMaximum write FMaximum;
    property Step: integer read FStep write FStep;
    property DefaultValue: integer read FDefaultValue write FDefaultValue;
    property FlagDisabled: boolean read FFlagDisabled write FFlagDisabled;
    property FlagGrabbed: boolean read FFlagGrabbed write FFlagGrabbed;
    property FlagReadOnly: boolean read FFlagReadOnly write FFlagReadOnly;
    property FlagUpdate: boolean read FFlagUpdate write FFlagUpdate;
    property FlagInactive: boolean read FFlagInactive write FFlagInactive;
    property FlagSlider: boolean read FFlagSlider write FFlagSlider;
    property FlagWriteOnly: boolean read FFlagWriteOnly write FFlagWriteOnly;
    property FlagVolatile: boolean read FFlagVolatile write FFlagVolatile;
    property Device: TVideo4L2Device read FDevice write FDevice;
    property Menu: TVideoControlMenu read FMenu write FMenu;
    property Value: integer read FValue write FValue;
    property DefaultValueIndex: integer read FDefaultValueIndex write FDefaultValueIndex;
  end;

  TVideoControlList = class (TCollection)
  private
    function GetItem(Index: integer): TVideoControl;
  public
    constructor Create;
    function Add: TVideoControl;
    property Item[Index: Integer]: TVideoControl read GetItem; default;
  end;

  TVideo4L2FrameEvent = procedure(Sender: TObject; Buffer: pointer; Size: integer; Error: boolean) of object;

  TVideo4L2CaptureThread = class(TThread)
  private
    Device: TVideo4L2Device;
    VideoBufferMemory: pointer;
    VideoBufferBytes: integer;
    VideoBufferError: boolean;
  protected
    procedure Execute; override;
    procedure CallOnFrameSynchronized;
  public
    constructor Create(ADevice: TVideo4L2Device);
    destructor Destroy; override;
  end;

  TVideoBufferRec = record
    mem: pointer;
    len: integer;
  end;

  TVideo4L2Device = class(TComponent)
  private
    FHandle: longint;
    VideoBuffer: array of TVideoBufferRec;
    CaptureThread: TVideo4L2CaptureThread;
    FOnFrame: TVideo4L2FrameEvent;
    FOnFrameSynchronized: TVideo4L2FrameEvent;
    FControlsInfo: TVideoControlList;
  private
    FDevice: string;
    FOpen: boolean;
    FCapture: boolean;
    FWidth: integer;
    FHeight: integer;
    FPixelFormat: integer;
    FFrameRate: integer;
    FBufferCount: integer;
    procedure SetDevice(ADevice: string);
    procedure SetOpen(AOpen: boolean);
    procedure SetCapture(ACapture: boolean);
    procedure SetWidth(AWidth: integer);
    procedure SetHeight(AHeight: integer);
    procedure SetPixelFormat(APixelFormat: integer);
    function GetFourCC:string;
    procedure SetFourCC(AFourCC: string);
    procedure SetFrameRate(AFrameRate: integer);
    procedure SetBufferCount(ABufferCount: integer);
  protected
    function DevIoctl(request: integer; data:pointer):integer;
    function DevMMap(len: integer; offset: integer):pointer;
    function DevMUnMap(mem: pointer; len: integer):integer;
    procedure UnmapBuffers;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  protected
    procedure OpenDevice;
    procedure CloseDevice;
    procedure SetFormat(AWidth,AHeight:integer;APixelFormat:integer);
    procedure SetStreamFrameRate(AFrameRate:integer);
    procedure InitBuffers(ABufferCount:integer);
    procedure CloseBuffers;
    procedure StartStream;
    procedure StopStream;
  public
    function GetControlValue(cid:integer):integer;
    procedure SetControlValue(cid,value:integer);
    procedure QueryControlsInfo;
    procedure GetControlValues;
    procedure SetDefaulControlValues;
    property ControlsInfo: TVideoControlList read FControlsInfo;
  public
    property Open: boolean read FOpen write SetOpen;
    property Capture: boolean read FCapture write SetCapture;
  published
    property Device: string read FDevice write SetDevice;
    property Width: integer read FWidth write SetWidth;
    property Height: integer read FHeight write SetHeight;
    property PixelFormat: integer read FPixelFormat write SetPixelFormat;
    property FourCC: string read GetFourCC write SetFourCC;
    property FrameRate: integer read FFrameRate write SetFrameRate;
    property BufferCount: integer read FBufferCount write SetBufferCount;
    property OnFrame: TVideo4L2FrameEvent read FOnFrame write FOnFrame;
    property OnFrameSynchronized: TVideo4L2FrameEvent read FOnFrameSynchronized write FOnFrameSynchronized;
  end;

procedure Register;

implementation

{$ifdef linux}
uses
  {$ifdef USELIBV4L2}
  libv4l2;
  {$else}
  BaseUnix;
  {$endif}
{$else}
{$WARNING Video4L2 is not avialiable for this platform!}
{$endif}

const
  O_RDWR = 2;
  PROT_READ = $1;
  MAP_SHARED = $1;
  MAP_FAILED = pointer(-1);

type
  // Type used for direct conversion of V4L2_PIX_FMT_* integer to Pascal string
  TFourCCArray = array [0..3] of char;

{ TVideoControlMenu }

constructor TVideoControlMenu.Create;
begin
  inherited Create(TVideoControlMenuItem);
end;

function TVideoControlMenu.Add: TVideoControlMenuItem;
begin
  Result := inherited Add as TVideoControlMenuItem;
end;

function TVideoControlMenu.GetItem(Index: integer): TVideoControlMenuItem;
begin
  Result := inherited Items[Index] as TVideoControlMenuItem;
end;

constructor TVideoControl.Create(ACollection: TCollection);
begin
  FDevice:=nil;
  FMenu:=TVideoControlMenu.Create;
  inherited Create(ACollection);
end;

destructor TVideoControl.Destroy;
begin
  FMenu.Free;
  inherited Destroy;
end;

function TVideoControl.FindMenuIndex(val: integer):integer;
var i:integer;
begin
  Result:=-1;
  for i:=0 to FMenu.Count-1 do begin
    if FMenu[i].Index=val then begin
      Result:=i;
      exit;
    end;
  end;
  raise EVideo4L2Exception.Create('Unable to find control '+Name+' id $'+IntToHex(Id,8)+' menu item with value index '+IntToStr(val));
end;

function TVideoControl.GetValue:integer;
var val:integer;
begin
  Result:=FDefaultValue;
  if Assigned(FDevice) then begin
    if ControlType in [V4L2_CTRL_TYPE_MENU, V4L2_CTRL_TYPE_INTEGER_MENU] then begin
      val:=FDevice.GetControlValue(Id);
      FValue:=FindMenuIndex(val);
    end else begin
      FValue:=FDevice.GetControlValue(Id);
    end;
    Result:=FValue;
  end;
end;

procedure TVideoControl.SetValue(val: integer);
begin
  if Assigned(FDevice) then begin
    if ControlType in [V4L2_CTRL_TYPE_MENU, V4L2_CTRL_TYPE_INTEGER_MENU] then begin
      FDevice.SetControlValue(Id,Menu[val].Index);
      FValue:=val;
    end else begin
      FDevice.SetControlValue(Id,val);
      FValue:=val;
    end;
  end;
end;

{ TVideoControlList }

constructor TVideoControlList.Create;
begin
  inherited Create(TVideoControl);
end;

function TVideoControlList.Add: TVideoControl;
begin
  Result := inherited Add as TVideoControl;
end;

function TVideoControlList.GetItem(Index: integer): TVideoControl;
begin
  Result := inherited Items[Index] as TVideoControl;
end;

{ TVideo4L2CaptureThread }

constructor TVideo4L2CaptureThread.Create(ADevice: TVideo4L2Device);
begin
  Device:=ADevice;
  inherited Create(True);
end;

destructor TVideo4L2CaptureThread.Destroy;
begin
  inherited Destroy;
end;

procedure TVideo4L2CaptureThread.CallOnFrameSynchronized;
begin
  Device.OnFrameSynchronized(Device,VideoBufferMemory,VideoBufferBytes,VideoBufferError);
end;

procedure TVideo4L2CaptureThread.Execute;
var
  vbuf: v4l2_buffer;
  i:integer;
begin
  try
    while not Terminated do begin
      FillChar({%H-}vbuf, SizeOf(vbuf), 0);
      vbuf._type := V4L2_BUF_TYPE_VIDEO_CAPTURE;
      vbuf.memory := V4L2_MEMORY_MMAP;

      // Wait for data
      if Device.DevIoctl(VIDIOC_DQBUF, @vbuf)<0 then begin
        if Terminated then begin
          break; // Dequeue request failed because we want to stop
        end else begin
          raise EVideo4L2Exception.Create('Capture Thread: Unable to dequeue buffer');
        end;
      end;

      VideoBufferMemory:=Device.VideoBuffer[vbuf.index].mem;
      VideoBufferBytes:=vbuf.bytesused;
      VideoBufferError:=((vbuf.flags and V4L2_BUF_FLAG_ERROR)<>0);
      // OnFrame runs in capture thread context
      if Assigned(Device.OnFrame) then begin
        Device.OnFrame(Device,VideoBufferMemory,VideoBufferBytes,VideoBufferError);
      end;
      // OnFrameSynchronized runs in main thread
      if Assigned(Device.OnFrameSynchronized) then begin
        Synchronize(@CallOnFrameSynchronized);
      end;

      // Re-submit the buffer
      if Device.DevIoctl(VIDIOC_QBUF, @vbuf)<0 then begin
        raise EVideo4L2Exception.Create('Capture Thread: Unable to queue buffer index '+IntToStr(vbuf.index));
      end;
    end;
  except
    on E: Exception do begin
      Writeln('Thread Execute exception');
      Writeln(E.Message);
      Writeln('Exception occurred at $',HexStr(ExceptAddr),':');
      Writeln(BackTraceStrFunc(ExceptAddr));
      if (ExceptFrameCount>0) then begin
        for i:=0 to ExceptFrameCount-1 do begin
          Writeln(BackTraceStrFunc(ExceptFrames[i]));
        end;
      end;
      Writeln;
      raise;
    end;
  end;
end;

{ TVideo4L2Device }

constructor TVideo4L2Device.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FDevice:='/dev/video0';
  SetLength(VideoBuffer,0);
  FOnFrame:=nil;
  FOnFrameSynchronized:=nil;
  CaptureThread:=nil;
  FControlsInfo:=TVideoControlList.Create;
  { component-level }
  FOpen:=False;
  FCapture:=False;
  FWidth:=640;
  FHeight:=480;
  FPixelFormat:=V4L2_PIX_FMT_YUYV;
  FFrameRate:=30;
  FBufferCount:=4;
end;

destructor TVideo4L2Device.Destroy;
begin
  {
  if Assigned(CaptureThread) then begin
    CaptureThread.FreeOnTerminate:=True;
    CaptureThread.Terminate;
  end;
  }

  //? How to safely terminate thread without waiting for it?
  {
    The problem arises not with thread execution itself,
    but with postponed Synronize'd method, which could be called
    after both the thread and owner object with buffers are destroyed.
  }
  StopStream; //! Too safe approach for rude .Destroy method

  // Exception-safe operations
  UnmapBuffers;
  CloseDevice;
  //
  FControlsInfo.Free;
  inherited Destroy;
end;

procedure TVideo4L2Device.OpenDevice;
begin
  {$ifdef linux}
  {$ifdef USELIBV4L2}
  FHandle:=v4l2_open(PChar(FDevice),O_RDWR);
  {$else}
  FHandle:=FpOpen(PChar(FDevice),O_RDWR);
  {$endif}
  if FHandle<0 then begin
    raise EVideo4L2Exception.Create('Unable to open video device '''+FDevice+'''');
  end;
  {$endif}
end;

procedure TVideo4L2Device.CloseDevice;
begin
  {$ifdef linux}
  if FHandle>=0 then begin
    {$ifdef USELIBV4L2}
    v4l2_close(FHandle);
    {$else}
    FpClose(FHandle);
    {$endif}
  end;
  {$endif}
  FControlsInfo.Clear;
end;

function TVideo4L2Device.DevIoctl(request: integer; data: pointer):integer;
begin
  Result:=-1;
  {$ifdef linux}
  if FHandle>=0 then begin
    {$ifdef USELIBV4L2}
    Result:=v4l2_ioctl(FHandle,request,data);
    {$else}
    Result:=FpIOCtl(FHandle,request,data);
    {$endif}
  end;
  {$endif}
end;

function TVideo4L2Device.DevMMap(len: integer; offset: integer):pointer;
begin
  Result:=MAP_FAILED;
  {$ifdef linux}
  if FHandle>=0 then begin
    {$ifdef USELIBV4L2}
    Result:=v4l2_mmap(nil, len, PROT_READ, MAP_SHARED, FHandle, offset);
    {$else}
    Result:=FpMMap(nil, len, PROT_READ, MAP_SHARED, FHandle, offset);
    {$endif}
  end;
  {$endif}
end;

function TVideo4L2Device.DevMUnMap(mem: pointer; len: integer):integer;
begin
  Result:=-1;
  {$ifdef linux}
  if FHandle>=0 then begin
    {$ifdef USELIBV4L2}
    Result:=v4l2_munmap(mem, len);
    {$else}
    Result:=FpMUnMap(mem, len);
    {$endif}
  end;
  {$endif}
end;

procedure TVideo4L2Device.SetFormat(AWidth,AHeight:integer;APixelFormat:integer);
var format: v4l2_format;
begin
  FillChar({%H-}format, SizeOf(format), 0);
  format._type := V4L2_BUF_TYPE_VIDEO_CAPTURE;
  format.fmt.pix.width := AWidth;
  format.fmt.pix.height := AHeight;
  format.fmt.pix.pixelformat := APixelFormat;
  format.fmt.pix.field := V4L2_FIELD_ANY;
  if DevIoctl(VIDIOC_S_FMT, @format)<0 then begin
    raise EVideo4L2Exception.Create('Unable to set video format '+IntToStr(AWidth)+'x'+IntToStr(AHeight)+' '+TFourCCArray(APixelFormat));
  end;
  // Update to actual values
  FWidth:=format.fmt.pix.width;
  FHeight:=format.fmt.pix.height;
  FPixelFormat:=format.fmt.pix.pixelformat;
end;

procedure TVideo4L2Device.SetStreamFrameRate(AFrameRate:integer);
var streamparm: v4l2_streamparm;
begin
  FillChar({%H-}streamparm, SizeOf(streamparm), 0);
  streamparm._type := V4L2_BUF_TYPE_VIDEO_CAPTURE;
  streamparm.parm.capture.timeperframe.numerator := 1;
  streamparm.parm.capture.timeperframe.denominator := AFrameRate;
  if DevIoctl(VIDIOC_S_PARM, @streamparm)<0 then begin
    raise EVideo4L2Exception.Create('Unable to set frame rate '+IntToStr(AFrameRate));
  end;
  if streamparm.parm.capture.timeperframe.numerator<>1 then begin
    raise EVideo4L2Exception.Create('Unsupported value obtained while setting FrameRate '+IntToStr(AFrameRate)+' (returned value is '+IntToStr(streamparm.parm.capture.timeperframe.numerator)+'/'+IntToStr(streamparm.parm.capture.timeperframe.denominator)+')');
  end;
  // Update to actual values
  FFrameRate:=streamparm.parm.capture.timeperframe.denominator;
end;

procedure TVideo4L2Device.UnmapBuffers;
var i:integer;
begin
  for i:=0 to Length(VideoBuffer)-1 do begin
    with VideoBuffer[i] do begin
      if Assigned(mem) then begin
        DevMUnMap(mem, len);
      end;
    end;
  end;
  SetLength(VideoBuffer,0);
end;

procedure TVideo4L2Device.InitBuffers(ABufferCount:integer);
var
  reqbuf: v4l2_requestbuffers;
  vbuf: v4l2_buffer;
  i:integer;
  map:pointer;
begin
  UnmapBuffers; // Unmap current buffers
  FillChar({%H-}reqbuf, SizeOf(reqbuf), 0);
  reqbuf.count := ABufferCount;
  reqbuf._type := V4L2_BUF_TYPE_VIDEO_CAPTURE;
  reqbuf.memory := V4L2_MEMORY_MMAP;
  if DevIoctl(VIDIOC_REQBUFS, @reqbuf)<0 then begin
    raise EVideo4L2Exception.Create('Unable to request '+IntToStr(ABufferCount)+' video buffers');
  end;
  if (ABufferCount>0) and (reqbuf.count=0) then begin
    raise EVideo4L2Exception.Create('Requested '+IntToStr(ABufferCount)+' video buffers, but got zero');
  end;
  if ABufferCount<>0 then begin
    FBufferCount:=reqbuf.count; // Update to actual value
  end;

  // Fill buffers with nil, to unmap only initialized buffers in case of exception
  SetLength(VideoBuffer,reqbuf.count);
  for i:=0 to reqbuf.count-1 do begin
    VideoBuffer[i].mem:=nil;
    VideoBuffer[i].len:=0;
  end;

  try
    for i:=0 to reqbuf.count-1 do begin
      FillChar({%H-}vbuf, SizeOf(vbuf), 0);
      vbuf.index := i;
      vbuf._type := V4L2_BUF_TYPE_VIDEO_CAPTURE;
      if DevIoctl(VIDIOC_QUERYBUF, @vbuf)<0 then begin
        raise EVideo4L2Exception.Create('Unable to query buffer index '+IntToStr(i)+' of '+IntToStr(reqbuf.count));
      end;

      map:=DevMMap(vbuf.length, vbuf.m.offset);
      if map=MAP_FAILED then begin
        raise EVideo4L2Exception.Create('Could not mmap video buffer index '+IntToStr(i)+' of '+IntToStr(reqbuf.count)+' length '+IntToStr(vbuf.length));
      end;
      VideoBuffer[i].mem:=map;
      VideoBuffer[i].len:=vbuf.length;
    end;

    for i:=0 to reqbuf.count-1 do begin
      FillChar(vbuf, SizeOf(vbuf), 0);
      vbuf.index := i;
      vbuf._type := V4L2_BUF_TYPE_VIDEO_CAPTURE;
      vbuf.memory := V4L2_MEMORY_MMAP;
      if DevIoctl(VIDIOC_QBUF, @vbuf)<0 then begin
        raise EVideo4L2Exception.Create('Unable to queue buffer index '+IntToStr(i)+' of '+IntToStr(reqbuf.count));
      end;
    end;
  except
    on E: Exception do begin
      // Unmap all mapped buffers on failure
      UnmapBuffers;
      raise;
    end
  end;
end;

procedure TVideo4L2Device.CloseBuffers;
begin
  InitBuffers(0); // Corresponing to V4L2 specs it is valid way to free all structures
end;

procedure TVideo4L2Device.StartStream;
var buftype: longint;
begin
  if not Assigned(CaptureThread) then begin
    buftype:=V4L2_BUF_TYPE_VIDEO_CAPTURE;
    if DevIoctl(VIDIOC_STREAMON, @buftype)<0 then begin
      raise EVideo4L2Exception.Create('Stream On failed');
    end;
    CaptureThread:=TVideo4L2CaptureThread.Create(Self);
    CaptureThread.Start;
  end;
end;

procedure TVideo4L2Device.StopStream;
var buftype: longint;
begin
  if Assigned(CaptureThread) then begin
    CaptureThread.Terminate;
    buftype:=V4L2_BUF_TYPE_VIDEO_CAPTURE;
    if DevIoctl(VIDIOC_STREAMOFF, @buftype)<0 then begin
      raise EVideo4L2Exception.Create('Stream Off failed');
    end;
    CaptureThread.WaitFor;
    FreeAndNil(CaptureThread);
  end;
end;

function TVideo4L2Device.GetControlValue(cid:integer):integer;
var control: v4l2_control;
begin
  control.id:=cid;
  if DevIoctl(VIDIOC_G_CTRL, @control)<0 then begin
    raise EVideo4L2Exception.Create('Could not Get Control id $'+IntToHex(cid,8));
  end;
  Result:=control.value;
end;

procedure TVideo4L2Device.SetControlValue(cid,value:integer);
var control: v4l2_control;
begin
  control.id:=cid;
  control.value:=value;
  if DevIoctl(VIDIOC_S_CTRL, @control)<0 then begin
    raise EVideo4L2Exception.Create('Could not Set Control id $'+IntToHex(cid,8)+' to value '+IntToStr(value));
  end;
end;

procedure TVideo4L2Device.QueryControlsInfo;
var
  queryctrl: v4l2_queryctrl;
  querymenu: v4l2_querymenu;
  id: longword;
  i: integer;
begin
  FControlsInfo.Clear;

  FillChar({%H-}queryctrl, SizeOf(queryctrl), 0);
  queryctrl.id := 0 or V4L2_CTRL_FLAG_NEXT_CTRL; // request next supported after 0 (V4L2_CID_BASE=$00980900)
  while DevIoctl(VIDIOC_QUERYCTRL, @queryctrl)=0 do begin
    with FControlsInfo.Add do begin
      Device:=Self;
      Id:=queryctrl.id;
      ControlType:=queryctrl._type;
      Name:=queryctrl.name;
      Minimum:=queryctrl.minimum;
      Maximum:=queryctrl.maximum;
      Step:=queryctrl.step;
      DefaultValue:=queryctrl.default_value;
      FlagDisabled:=(queryctrl.flags and V4L2_CTRL_FLAG_DISABLED)<>0;
      FlagGrabbed:=(queryctrl.flags and V4L2_CTRL_FLAG_GRABBED)<>0;
      FlagReadOnly:=(queryctrl.flags and V4L2_CTRL_FLAG_READ_ONLY)<>0;
      FlagUpdate:=(queryctrl.flags and V4L2_CTRL_FLAG_UPDATE)<>0;
      FlagInactive:=(queryctrl.flags and V4L2_CTRL_FLAG_INACTIVE)<>0;
      FlagSlider:=(queryctrl.flags and V4L2_CTRL_FLAG_SLIDER)<>0;
      FlagWriteOnly:=(queryctrl.flags and V4L2_CTRL_FLAG_WRITE_ONLY)<>0;
      FlagVolatile:=(queryctrl.flags and V4L2_CTRL_FLAG_VOLATILE)<>0;

      if (queryctrl._type=V4L2_CTRL_TYPE_MENU) or (queryctrl._type=V4L2_CTRL_TYPE_INTEGER_MENU) then begin
        DefaultValueIndex:=queryctrl.minimum-1;
        for i:=queryctrl.minimum to queryctrl.maximum do begin
          FillChar({%H-}querymenu, SizeOf(querymenu), 0);
          querymenu.id:=queryctrl.id;
          querymenu.index:=i;
          if DevIoctl(VIDIOC_QUERYMENU, @querymenu)=0 then begin
            with Menu.Add do begin
              Index:=querymenu.index;
              if Index=DefaultValue then begin
                DefaultValueIndex:=Menu.Count-1;
              end;
              if queryctrl._type=V4L2_CTRL_TYPE_INTEGER_MENU then begin
                TypeInteger:=True;
                Int64Value:=querymenu.u.value;
                Name:=IntToStr(querymenu.u.value);
              end else begin
                TypeInteger:=False;
                Name:=querymenu.u.name;
              end;
            end;
          end;
        end;
        if DefaultValueIndex=(queryctrl.minimum-1) then begin
          //? Default menu value not found in menu list
        end;
        Value:=DefaultValueIndex;
      end else begin
        Value:=DefaultValue;
        DefaultValueIndex:=DefaultValue;
      end;
    end;
    id:=queryctrl.id or V4L2_CTRL_FLAG_NEXT_CTRL; // request next supported after this id
    FillChar({%H-}queryctrl, SizeOf(queryctrl), 0);
    queryctrl.id:=id;
  end;
end;

procedure TVideo4L2Device.GetControlValues;
var
  i: integer;
begin
  for i:=0 to ControlsInfo.Count-1 do begin
    try
      ControlsInfo[i].GetValue;
    except
      on E: EVideo4L2Exception do begin
        // ignore read exceptions
      end;
    end;
  end;
end;

procedure TVideo4L2Device.SetDefaulControlValues;
var
  i: integer;
begin
  for i:=0 to ControlsInfo.Count-1 do begin
    try
      with ControlsInfo[i] do begin
        SetValue(DefaultValueIndex);
      end;
    except
      on E: EVideo4L2Exception do begin
        // ignore write exceptions
      end;
    end;
  end;
end;

{ component-level logic }

procedure TVideo4L2Device.SetDevice(ADevice: string);
begin
  if ADevice<>FDevice then begin
    SetOpen(False);
    FDevice:=ADevice;
    // Do not reopen by default
  end;
end;

procedure TVideo4L2Device.SetOpen(AOpen: boolean);
begin
  if AOpen<>FOpen then begin
    if AOpen then begin
      OpenDevice;
      FOpen:=True;
      SetFormat(FWidth,FHeight,FPixelFormat);
      SetStreamFrameRate(FFrameRate);
      QueryControlsInfo;
    end else begin
      try
        StopStream; // stop streaming if needed
      finally
        FCapture:=False;
        //CloseBuffers; //? do we need to call it on close, or mere UnmapBuffers will suffice?
        UnmapBuffers; // exception-safe
        CloseDevice;
        FOpen:=False;
      end;
    end;
  end;
end;

procedure TVideo4L2Device.SetCapture(ACapture: boolean);
begin
  if ACapture<>FCapture then begin
    if ACapture then begin
      SetOpen(True);
      SetFormat(FWidth,FHeight,FPixelFormat);
      SetStreamFrameRate(FFrameRate);
      InitBuffers(FBufferCount);
      StartStream;
      FCapture:=True;
    end else begin
      StopStream;
      CloseBuffers;
      FCapture:=False;
    end;
  end;
end;

procedure TVideo4L2Device.SetWidth(AWidth: integer);
begin
  if AWidth<>FWidth then begin
    SetCapture(False);
    FWidth:=AWidth;
  end;
end;

procedure TVideo4L2Device.SetHeight(AHeight: integer);
begin
  if AHeight<>FHeight then begin
    SetCapture(False);
    FHeight:=AHeight;
  end;
end;

procedure TVideo4L2Device.SetPixelFormat(APixelFormat: integer);
begin
  if APixelFormat<>FPixelFormat then begin
    SetCapture(False);
    FPixelFormat:=APixelFormat;
  end;
end;

function TVideo4L2Device.GetFourCC: string;
begin
  Result:=TFourCCArray(FPixelFormat);
end;

procedure TVideo4L2Device.SetFourCC(AFourCC: string);
begin
  if Length(AFourCC)<>4 then begin
    raise EVideo4L2Exception.Create('Invalid FourCC code, should be string of length 4');
  end;
  SetPixelFormat(PInteger(@AFourCC[1])^);
end;

procedure TVideo4L2Device.SetFrameRate(AFrameRate: integer);
var capturing:boolean;
begin
  if AFrameRate<>FFrameRate then begin
    if Open then begin
      capturing:=Capture;
      SetCapture(False);
      SetStreamFrameRate(AFrameRate);
      SetCapture(capturing);
    end else begin
      FFrameRate:=AFrameRate;
    end;
  end;
end;

procedure TVideo4L2Device.SetBufferCount(ABufferCount: integer);
var capturing:boolean;
begin
  if ABufferCount<>FBufferCount then begin
    if Open then begin
      capturing:=Capture;
      SetCapture(False);
      FBufferCount:=ABufferCount;
      SetCapture(capturing);
    end else begin
      FBufferCount:=ABufferCount;
    end;
  end;
end;

procedure Register;
begin
  RegisterComponents('Video4L2',[TVideo4L2Device]);
end;

end.

