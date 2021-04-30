{ This file was automatically created by Lazarus. Do not edit!
  This source is only used to compile and install the package.
 }

unit LazV4LCapture;

{$warn 5023 off : no warning about unused units}
interface

uses
  videodev2, VideoCapture, libv4l2, uvcvideo, v4l2_subdev, LazarusPackageIntf;

implementation

procedure Register;
begin
  RegisterUnit('VideoCapture', @VideoCapture.Register);
end;

initialization
  RegisterPackage('LazV4LCapture', @Register);
end.
