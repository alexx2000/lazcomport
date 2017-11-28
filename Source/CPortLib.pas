{ This file was automatically created by Lazarus. Do not edit!
  This source is only used to compile and install the package.
 }

unit CPortLib;

interface

uses
  CPort, CPortSetup, CPortCtl, CPortEsc, CPortTrmSet, LazarusPackageIntf;

implementation

procedure Register;
begin
end;

initialization
  RegisterPackage('CPortLib', @Register);
end.
