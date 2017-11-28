{ This file was automatically created by Lazarus. Do not edit!
  This source is only used to compile and install the package.
 }

unit DsgnCPort;

interface

uses
  CPortReg, CPortAbout, LazarusPackageIntf;

implementation

procedure Register;
begin
  RegisterUnit('CPortReg', @CPortReg.Register);
end;

initialization
  RegisterPackage('DsgnCPort', @Register);
end.
