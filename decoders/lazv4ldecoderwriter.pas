{ This file was automatically created by Lazarus. Do not edit!
  This source is only used to compile and install the package.
 }

unit LazV4LDecoderWriter;

{$warn 5023 off : no warning about unused units}
interface

uses
  RGB16writer, RGB24writer, RGB32writer, SRGGB82RGB, YUV2RGB, 
  LazarusPackageIntf;

implementation

procedure Register;
begin
end;

initialization
  RegisterPackage('LazV4LDecoderWriter', @Register);
end.
