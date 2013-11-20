{ This file was automatically created by Lazarus. Do not edit!
  This source is only used to compile and install the package.
 }

unit LazVideoCapture;

interface

uses
  videodev2, VideoCapture, YUV2RGB, libv4l2, LazarusPackageIntf;

implementation

procedure Register;
begin
  RegisterUnit('VideoCapture', @VideoCapture.Register);
end;

initialization
  RegisterPackage('LazVideoCapture', @Register);
end.
