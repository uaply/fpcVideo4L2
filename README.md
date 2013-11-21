# Video4L2 capture and controls enumeration component for Lazarus

### Version 1.0.1

Copyright (C) 2013 Yuriy Pilgun

## Usage

See provided demo project.

### OnFrame event

There are two events `OnFrame` and `OnFrameSynchronized`. The first `OnFrame` runs in context of Capturing thread, so you can't draw on Form inside it, but you can convert data if you wish. To draw data on Form you should use `OnFrameSynchronized`, which is executed in context of the main thread.

### libv4l2

You can try to set `{$define USELIBV4L2}` to use `libv4l2.so` wrapper library. It has some advantages such as providing automatic color space conversion to RGB24 (BGR24) or YUV420 (YVU420) for cameras that don't support them natively.

## Caveats

Make sure you don't destroy objects needed to draw a frame before stopping video capture thread. If you placed `TVideo4L2Device` on a form it will be destroyed automatically, but at wrong time. You probably will need some temporary bitmap to draw frames from `OnFrameSynchronized` event. If you free this temporary bitmap in `TForm.OnDestroy` event, it will be destroyed BEFORE stopping video, and synchronized call from capture thread will get Access Violation exception. To fix this issue free video object explicitly:
```pascal
procedure TForm1.FormDestroy(Sender: TObject);
begin
  Video4L2Device1.Free; // <- this will stop capture thread before destroying other objects
  TempBitmap.Free;
end;

```

## License

This program is provided under the terms of the MIT License.
